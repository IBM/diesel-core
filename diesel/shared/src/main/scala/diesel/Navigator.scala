/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package diesel

import diesel.Navigator.Ambiguity
import diesel.Bnf.{DslElement, Production}
import diesel.Errors.Ambiguous
import diesel.Lexer.Token

import scala.collection.mutable

private[diesel] class ParsingContext(
  val userDataProvider: Option[UserDataProvider],
  val begin: Int,
  val end: Int,
  val offset: Int,
  val length: Int,
  val ambiguity: Option[Ambiguity],
  val markers: Seq[Marker],
  val children: Seq[GenericNode]
) extends Context {

  override def getUserData(key: Any): Option[Any] = userDataProvider.flatMap(_.getUserData(key))

  override def setUserData(key: Any, value: Any): Unit =
    userDataProvider.foreach(_.setUserData(key, value))

  private var locals: Seq[Marker]              = Seq()
  private var style: Option[Style]             = None
  private var tokenStyles: Seq[(Token, Style)] = Seq()
  private var aborted: Boolean                 = children.exists(child => child.context.hasAborted)

  override def addMarkers(marker: Marker, markers: Marker*): Unit = {
    locals = locals ++ Seq(marker) ++ markers
  }

  def reportErrors(): Seq[Marker] = locals

  def setStyle(style: Style): Unit = {
    this.style = Some(style)
  }

  def setTokenStyle(token: Token, style: Style): Unit = {
    tokenStyles = tokenStyles ++ Seq((token, style))
  }

  override def getStyle: Option[Style] = style

  override def getTokenStyles: Seq[(Token, Style)] = tokenStyles.map(ts => (ts._1, ts._2))

  override def hasAborted: Boolean = aborted

  override def abort(): Boolean = {
    if (!aborted) {
      ambiguity.foreach(_.abort())
      aborted = true
    }
    aborted
  }
}

private[diesel] case class Parsing(
  node: GenericNode,
  value: Any,
  offset: Int,
  length: Int,
  markers: Seq[Marker]
)

object GenericTree {

  def asIterator(root: GenericNode, descendants: Boolean): Iterator[GenericNode] = {
    asIterator(Seq(root), descendants)
  }

  def asIterator(
    roots: Seq[GenericNode],
    descendants: Boolean
  ): Iterator[GenericNode] with Object {} = {
    val processingQueue: mutable.Queue[GenericNode] = mutable.Queue()
    roots.foreach(child => processingQueue.enqueue(child))
    new Iterator[GenericNode] {
      override def hasNext: Boolean = processingQueue.nonEmpty

      override def next(): GenericNode = {
        val res = processingQueue.dequeue()
        if (descendants) {
          res.getChildren.foreach(child => processingQueue.enqueue(child))
        }
        res
      }
    }
  }

  def asIterable(root: GenericNode, descendants: Boolean): Iterable[GenericNode] = {
    asIterable(Seq(root), descendants)
  }

  def asIterable(
    roots: Seq[GenericNode],
    descendants: Boolean
  ): Iterable[GenericNode] with Object {} = {
    new Iterable[GenericNode] {
      override def iterator: Iterator[GenericNode] = asIterator(roots, descendants)
    }
  }
}

case class GenericTree(
  root: GenericNode,
  value: Any,
  offset: Int,
  length: Int,
  markers: Seq[Marker]
) {

  def toIterator: Iterator[GenericNode] = {
    GenericTree.asIterator(root, descendants = true)
  }

  def toIterable: Iterable[GenericNode] = {
    GenericTree.asIterable(root, descendants = true)
  }

  def toSeq: Seq[GenericNode] = toIterable.toSeq

  override def toString: String = prettyPrint(this.root, 0).mkString("\n")

  private def nodeToStr(node: GenericNode): String =
    (node.toString + (if (node.value == null) "" else " => " + node.value.toString))

  private def prettyPrint(node: GenericNode, indent: Int): Seq[String] = {
    Seq(
      (" " * indent) + nodeToStr(node)
    ) ++ node.getChildren.flatMap(c => prettyPrint(c, indent + 1))
  }

}

abstract class GenericNode(var parent: Option[GenericNode], val context: Context, val value: Any) {
  private[diesel] def toString(buf: mutable.StringBuilder): Unit

  def offset: Int = context.offset

  def length: Int = context.length

  def valueAs[T]: T = value.asInstanceOf[T]

  def hasAmbiguity: Boolean = false

  override def toString: String = {
    val buf = new mutable.StringBuilder
    toString(buf)
    buf.toString()
  }

  def getChildren: Seq[GenericNode] = Seq()

  def toIterator(descendants: Boolean = false): Iterator[GenericNode] = {
    GenericTree.asIterator(getChildren, descendants)
  }

  def toIterable(descendants: Boolean = false): Iterable[GenericNode] = {
    GenericTree.asIterable(getChildren, descendants)
  }

  def toSeq(descendants: Boolean = false): Seq[GenericNode] = toIterable(descendants).toSeq

  def findNodeAtIndex(index: Int): Option[GenericNode] =
    if (containsIndex(index)) {
      getChildren.foreach(child => {
        child.findNodeAtIndex(index) match {
          case Some(value) => return Some(value)
          case _           =>
        }
      })
      if (startsAtIndex(index))
        Some(this)
      else
        None
    } else
      None

  def startsAtIndex(index: Int): Boolean =
    context.begin == index

  def containsIndex(index: Int): Boolean =
    (context.begin <= index && index <= context.end)

  def isAtOffset(atOffset: Int): Boolean =
    (atOffset == offset && length == 0) || (atOffset >= offset && atOffset < offset + length)

  def findNodesAtOffset(offset: Int): Iterable[GenericNode] =
    toIterable(true)
      .filter(_.isAtOffset(offset))

  def findFirstParent(p: GenericNode => Boolean): Option[GenericNode] = {
    parent match {
      case Some(parent) =>
        if (p(parent))
          Some(parent)
        else
          parent.findFirstParent(p)
      case None         =>
        None
    }
  }

  def getParents: Seq[GenericNode] = {
    parent match {
      case Some(p) =>
        Seq(p) ++ p.parent.map(_.getParents).getOrElse(Seq.empty)
      case None    =>
        Seq.empty
    }
  }

  def getElement: Option[DslElement] = None
}

class GenericNonTerminal(
  override val context: Context,
  val production: Production,
  val children: Seq[GenericNode],
  override val value: Any
) extends GenericNode(None, context, value) {

  children.foreach(child => child.parent = Some(this))

  override def getChildren: Seq[GenericNode] = children

  override def hasAmbiguity: Boolean =
    getChildren.exists(child =>
      child.context match {
        case parsingCtx: ParsingContext => parsingCtx.ambiguity.isDefined
        case _                          => false
      }
    )

  private[diesel] def toString(buf: mutable.StringBuilder): Unit = {
    // TODO useful for debugging?
//    buf.append("[").append(production.element.getOrElse("?")).append("]")
    buf.append(production.rule.get.name).append("(")
    buf.append(context.offset).append(", ").append(context.length)
    children.zipWithIndex.foreach {
      case (child: GenericNode, index: Int) =>
        buf.append(", ").append(child)
    }
    buf.append(")")
  }

  override def getElement: Option[DslElement] = production.element

}

class GenericTerminal(override val context: Context, val token: Token)
    extends GenericNode(None, context, token) {

  private[diesel] def toString(buf: StringBuilder): Unit = {
    buf.append(token.id.name).append("(").append(token.text).append(")")
  }
}

private object GenericSentinel
    extends GenericNode(
      None,
      new ParsingContext(None, -1, -1, 0, -1, None, Seq(), Seq()),
      None
    ) {

  private[diesel] def toString(buf: StringBuilder): Unit = {
    buf.append("Sentinel()")
  }
}

object Navigator {

  def apply(
    result: Result,
    postProcessors: Seq[GenericTree => Seq[Marker]] = Seq.empty,
    userDataProvider: Option[UserDataProvider] = None
  ): Navigator =
    new Navigator(result, postProcessors, userDataProvider)

  def select(navigator: Navigator): Option[GenericTree] = {
    var asts: Seq[GenericTree] = Seq()
    var errorCount: Int        = Int.MaxValue
    while (navigator.hasNext) {
      val candidate: GenericTree = navigator.next()
      if (asts.isEmpty)
        asts = Seq(candidate)
      else {
        val actualErrorCount = Marker.countErrors(candidate.markers)
        if (errorCount > actualErrorCount)
          asts = Seq(candidate)
        else if (errorCount == 0 && actualErrorCount == 0)
          asts = asts ++ Seq(candidate)
      }
      errorCount = Marker.countErrors(asts.head.markers)
    }
    if (errorCount == 0 && asts.size > 1) {
      val ast                 = asts.head
      var errors: Seq[Marker] = Seq()
      ast.toSeq.foreach(node =>
        if (node.hasAmbiguity)
          errors = errors ++ Seq(Ambiguous.apply(node.offset, node.length))
      )
      if (errors.isEmpty)
        errors = Seq(Ambiguous.apply(ast.offset, ast.length))
      Some(GenericTree(ast.root, ast.value, ast.offset, ast.length, ast.markers ++ errors))
    } else asts.headOption
  }

  private[diesel] class Ambiguity(val branchCount: Int) {

    private var abortedBranchCount: Int = 0

    private[diesel] def abort(): Unit = abort(1)

    private[diesel] def abort(count: Int): Unit = abortedBranchCount += count

    private[diesel] def ambiguous: Boolean = branchCount - abortedBranchCount > 1
  }
}

class Navigator(
  val result: Result,
  val postProcessors: Seq[GenericTree => Seq[Marker]],
  private val userDataProvider: Option[UserDataProvider]
) {

  private val root: Iterator[Seq[Parsing]] = nonTerminal(result.successState)

  def hasNext: Boolean = root.hasNext

  def next(): GenericTree = {
    val current = root.next().head
    var tree    =
      GenericTree(
        current.node,
        current.value,
        current.offset,
        current.length,
        current.markers ++ result.reportErrors()
      )
    if (postProcessors.nonEmpty) {
      var markers: Seq[Marker] = Seq()
      postProcessors.foreach(pp => markers = markers ++ pp(tree))
      tree = GenericTree(tree.root, tree.value, tree.offset, tree.length, tree.markers ++ markers)
    }
    tree
  }

  def toIterator: Iterator[GenericTree] with Object {} = {
    val nav: Navigator = this
    new Iterator[GenericTree] {
      override def hasNext: Boolean = nav.hasNext

      override def next(): GenericTree = nav.next()
    }
  }

  private def sentinel(): Iterator[Seq[Parsing]] = Seq(Seq()).iterator

  private def terminal(item: TerminalItem): Iterator[Seq[Parsing]] =
    Seq(Seq(applyToken(item))).iterator

  private def nonTerminal(state: State): Iterator[Seq[Parsing]] =
    new StateIterator(state)

  private def collectSubtrees(state: State, value: Iterator[Seq[Parsing]]): Seq[Seq[Parsing]] = {
    var subtrees: Seq[Seq[Parsing]] = Seq()
    value.foreach(subtree =>
      if (
        state.production.isEmpty || !subtree.head.node.context.hasAborted || (subtrees.isEmpty && !value.hasNext)
      ) {
        subtrees = subtrees :+ subtree
      }
    )
    subtrees
  }

  private def applyToken(terminal: TerminalItem): Parsing = {
    val errors = terminal.reportErrors()
    val token  = terminal.token
    val node   = new GenericTerminal(
      new ParsingContext(
        userDataProvider,
        terminal.begin,
        terminal.end,
        token.offset,
        token.text.length,
        None,
        terminal.reportErrors(),
        Seq()
      ),
      token
    )
    terminal match {
      case InsertedTokenValue(_, _, _) =>
        Parsing(node, terminal, token.offset, 0, errors)
      case _                           =>
        Parsing(node, terminal, token.offset, token.text.length, terminal.reportErrors())
    }
  }

  private def applyRule(
    production: Production,
    children: Seq[GenericNode],
    args: IndexedSeq[Any],
    begin: Int,
    end: Int,
    offset: Int,
    length: Int,
    ambiguity: Option[Ambiguity],
    styles: Seq[(Token, Style)],
    errors: Seq[Marker]
  ): Parsing = {
    val context = new ParsingContext(
      userDataProvider,
      begin,
      end,
      offset,
      length,
      ambiguity,
      errors,
      children
    )
    val value   = production.action(context, args)
    styles.foreach(pair => context.setTokenStyle(pair._1, pair._2))
    Parsing(
      new GenericNonTerminal(context, production, children, value),
      value,
      offset,
      length,
      errors ++ context.reportErrors()
    )
  }

  private def reduceState(
    state: State,
    stack: Seq[Parsing],
    ambiguity: Option[Ambiguity]
  ): Parsing = {
    var children: Seq[GenericNode]  = IndexedSeq()
    var args: IndexedSeq[Any]       = IndexedSeq()
    var i                           = state.production.length
    var styles: Seq[(Token, Style)] = Seq()
    var errors: Seq[Marker]         = Seq()
    var index                       = stack
    while (i > 0) {
      val value = index.head
      value.value match {
        case InsertedTokenValue(_, _, _) => /* Ignore */
        case item: TerminalItem          =>
          children = value.node +: children
          args = item.token +: args
          item.style.foreach(s => styles = (item.token, s) +: styles)
          i -= 1
        case _                           =>
          children = value.node +: children
          args = value.value +: args
          i -= 1
      }
      errors = value.markers ++ errors
      index = index.tail
    }
    while (index.nonEmpty) {
      val value = index.head
      value.value match {
        case InsertedTokenValue(_, _, _) =>
          errors = value.markers ++ errors
        case _                           =>
          throw new RuntimeException()
      }
      index = index.tail
    }
    val offset                      = result.tokenAt(state.begin).map(_.offset).getOrElse(-1)
    val length                      =
      if (state.begin == state.end) {
        0
      } else {
        result.tokenAt(state.end - 1).map(tk => tk.offset + tk.text.length).getOrElse(-1) - offset
      }
    applyRule(
      state.production,
      children,
      args,
      state.begin,
      state.end,
      offset,
      length,
      ambiguity,
      styles,
      errors
    )
  }

  private class BackPtrIterator(
    causal: Iterator[Seq[Parsing]],
    predecessor: State
  ) extends Iterator[Seq[Parsing]] {

    private var current      = if (causal.hasNext) causal.next() else Seq()
    private var predIterator = nonTerminal(predecessor)
    private var finished     = false

    override def next(): Seq[Parsing] = {
      if (!predIterator.hasNext && finished) {
        throw new NoSuchElementException()
      }
      val result = current ++ predIterator.next()
      if (!predIterator.hasNext) {
        if (causal.hasNext) {
          current = causal.next()
          predIterator = nonTerminal(predecessor)
        } else
          finished = true
      }
      result
    }

    override def hasNext: Boolean = predIterator.hasNext || causal.hasNext
  }

  private class StateIterator(val state: State)
      extends Iterator[Seq[Parsing]] {

    private val backPtrs: Seq[BackPtr]                  =
      Navigator.this.result.contextOf(state).fold[Seq[BackPtr]](Seq())(ctx => ctx.backPtrs.toSeq)
    private val ambiguity: Option[Ambiguity]            =
      if (backPtrs.size > 1) Some(new Ambiguity(backPtrs.size)) else None
    private var index: Seq[BackPtr]                     = backPtrs
    private var current: Option[Iterator[Seq[Parsing]]] = moveToNext(true)

    private def alternative(
      causal: Iterator[Seq[Parsing]],
      predecessor: State
    ): Iterator[Seq[Parsing]] =
      new BackPtrIterator(causal, predecessor)

    private def moveToNext(first: Boolean): Option[Iterator[Seq[Parsing]]] = {
      val res = if (index.isEmpty) {
        if (first) Some(sentinel()) else None
      } else {
        val backPtr = index.head
        index = index.tail
        val causal  = backPtr.causal match {
          case item: TerminalItem => terminal(item)
          case state: State       => nonTerminal(state)
        }
        Some(alternative(causal, backPtr.predecessor))
      }
      if (state.isCompleted) {
        res match {
          case Some(value) =>
            val subtrees = collectSubtrees(state, value)
            Some(subtrees.iterator)
          case None        => None
        }
      } else
        res
    }

    override def next(): Seq[Parsing] = {
      current match {
        case Some(value) =>
          val res = if (state.isCompleted) {
            Seq(reduceState(state, value.next(), ambiguity))
          } else {
            value.next()
          }
          if (!value.hasNext) {
            current = moveToNext(false)
          }
          res
        case None        =>
          throw new NoSuchElementException()
      }
    }

    override def hasNext: Boolean = current match {
      case Some(value) => value.hasNext || index.nonEmpty
      case None        => false
    }
  }
}
