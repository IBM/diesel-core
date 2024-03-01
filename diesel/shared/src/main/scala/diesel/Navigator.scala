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
  val userData: ContextualUserData,
  val begin: Int,
  val end: Int,
  val offset: Int,
  val length: Int,
  val ambiguity: Option[Ambiguity],
  val children: Seq[GenericNode]
) extends Context {

  override def getUserData(key: Any): Option[Any] = userData.getUserData(key)

  override def setUserData(key: Any, value: Any): Unit = userData.setUserData(key, value)

  private var locals: Seq[Marker]              = Seq.empty
  private var style: Option[Style]             = None
  private var tokenStyles: Seq[(Token, Style)] = Seq.empty
  private var aborted: Boolean                 = children.exists(child => child.context.hasAborted)

  override def markers: Seq[Marker] = locals

  override def addMarkers(marker: Marker, markers: Marker*): Unit = {
    locals = locals ++ Seq(marker) ++ markers
  }

  def setStyle(style: Style): Unit = {
    this.style = Some(style)
  }

  def setTokenStyle(token: Token, style: Style): Unit = {
    tokenStyles = tokenStyles ++ Seq((token, style))
  }

  override def getStyle: Option[Style] = style

  override def getTokenStyles: Seq[(Token, Style)] = tokenStyles.map(ts => (ts._1, ts._2))

  override def hasAborted: Boolean = aborted

  override def abort(): Unit = {
    aborted = true
  }
}

private[diesel] case class Parsing(
  node: GenericNode,
  value: Any,
  offset: Int,
  length: Int,
  markers: Seq[Marker]
) {
  def asAmbiguous(): Parsing = Parsing(
    node,
    value,
    offset,
    length,
    markers ++ Seq(Ambiguous.apply(node.offset, node.length))
  )
}

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
    node.toString + (if (node.value == null) "" else " => " + node.value.toString)

  private def prettyPrint(node: GenericNode, indent: Int): Seq[String] = {
    Seq(
      (" " * indent) + nodeToStr(node)
    ) ++ node.getChildren.flatMap(c => prettyPrint(c, indent + 1))
  }

}

abstract class GenericNode(var parent: Option[GenericNode], val context: Context, val value: Any) {
  private[diesel] def toString(buf: mutable.StringBuilder): mutable.StringBuilder

  def offset: Int = context.offset

  def length: Int = context.length

  def valueAs[T]: T = value.asInstanceOf[T]

  def localMarkers: Seq[Marker] = context.markers

  def markers: Seq[Marker] = localMarkers

  def hasAmbiguity: Boolean = false

  def wasAmbiguous: Boolean = false

  override def toString: String = {
    val buf = new mutable.StringBuilder
    toString(buf)
    buf.toString()
  }

  def getChildren: Seq[GenericNode] = Seq.empty

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
    context.begin <= index && index <= context.end

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
  override val markers: Seq[Marker],
  val production: Production,
  override val value: Any
) extends GenericNode(None, context, value) {

  context.children.foreach(child => child.parent = Some(this))

  override def getChildren: Seq[GenericNode] = context.children

  override def hasAmbiguity: Boolean =
    context match {
      case c: ParsingContext => c.ambiguity match {
          case Some(a) => a.ambiguous
          case None    => false
        }
      case _                 => false
    }

  override def wasAmbiguous: Boolean =
    context match {
      case c: ParsingContext => c.ambiguity match {
          case Some(a) => a.branchCount > 1
          case None    => false
        }
      case _                 => false
    }

  private[diesel] def toString(buf: mutable.StringBuilder): mutable.StringBuilder = {
    // TODO useful for debugging?
//    buf.append("[").append(production.element.getOrElse("?")).append("]")
    buf.append(production.rule.get.name).append("(")
    buf.append(context.offset).append(", ").append(context.length)
    context.children.zipWithIndex.foreach {
      case (child: GenericNode, _: Int) =>
        buf.append(", ").append(child)
    }
    buf.append(")")
  }

  override def getElement: Option[DslElement] = production.element

}

class GenericTerminal(override val context: Context, val token: Token)
    extends GenericNode(None, context, token) {

  private[diesel] def toString(buf: mutable.StringBuilder): mutable.StringBuilder = {
    buf.append(token.id.name).append("(").append(token.text).append(")")
  }
}

object Navigator {

  val defaultReducer: Seq[GenericNode => Reducer] =
    Seq(Reducer.noAbortAsMuchAsPossible, Reducer.selectOne)

  def apply(
    result: Result,
    postProcessors: Seq[GenericTree => Seq[Marker]] = Seq.empty,
    reducer: Seq[GenericNode => Reducer] = defaultReducer,
    userDataProvider: Option[UserDataProvider] = None
  ): Navigator =
    new Navigator(result, postProcessors, reducer, userDataProvider)

  private[diesel] class Ambiguity(val branchCount: Int) {

    private var abortedBranchCount: Int = 0

    private[diesel] def abort(): Unit = abort(1)

    private[diesel] def abort(count: Int): Unit = abortedBranchCount += count

    private[diesel] def ambiguous: Boolean =
      branchCount - abortedBranchCount > 1
  }

  type Filter = Seq[GenericNode] => Seq[GenericNode]
}

trait ContextualUserData {

  def getUserData(key: Any): Option[Any]

  def setUserData(key: Any, value: Any): Unit

  def toLocalUserData(): LocalUserData
}

case class GlobalUserData(userDataProvider: Option[UserDataProvider]) extends ContextualUserData {

  override def getUserData(key: Any): Option[Any] = userDataProvider.flatMap(_.getUserData(key))

  override def setUserData(key: Any, value: Any): Unit =
    userDataProvider.foreach(_.setUserData(key, value))

  override def toLocalUserData(): LocalUserData = LocalUserData(userDataProvider)
}

case class LocalUserData(userDataProvider: Option[UserDataProvider]) extends ContextualUserData {

  private var data: Map[Any, Any] = Map()

  override def getUserData(key: Any): Option[Any] =
    if (data.contains(key)) data.get(key) else userDataProvider.flatMap(_.getUserData(key))

  override def setUserData(key: Any, value: Any): Unit = {
    data = data + (key -> value)
  }

  override def toLocalUserData(): LocalUserData = {
    val res = LocalUserData(userDataProvider)
    this.data foreach { entry =>
      res.setUserData(entry._1, entry._2)
    }
    res
  }
}

case class Subtree(stack: Seq[Parsing])

case class Subtrees(choices: Iterator[Subtree], userData: ContextualUserData)

trait Reducer {

  def node: GenericNode

  def compare(node: GenericNode): (Reducer.Kind.Kind, Reducer)

  def close(subtrees: Seq[Subtree]): Seq[Subtree] = subtrees
}

object Reducer {

  object Kind extends Enumeration {
    type Kind = Value
    val Better, Same, Worse = Value
  }

  case class FewerErrorPossible(override val node: GenericNode) extends Reducer {

    private val errorCount = Marker.countErrors(node.markers)

    override def compare(node: GenericNode): (Reducer.Kind.Kind, Reducer) = {
      val other = FewerErrorPossible(node)
      if (other.errorCount < this.errorCount) {
        (Reducer.Kind.Better, other)
      } else if (other.errorCount > this.errorCount) {
        (Reducer.Kind.Worse, this)
      } else if (this.errorCount == 0) {
        (Reducer.Kind.Same, this)
      } else {
        (Reducer.Kind.Worse, this)
      }
    }
  }

  def fewerErrorPossible: GenericNode => Reducer =
    (node: GenericNode) => FewerErrorPossible(node)

  case class NoAbortAsMushAsPossible(override val node: GenericNode) extends Reducer {

    override def compare(node: GenericNode): (Reducer.Kind.Kind, Reducer) = {
      val other = NoAbortAsMushAsPossible(node)
      if (this.node.context.hasAborted) {
        (Reducer.Kind.Better, other)
      } else if (other.node.context.hasAborted) {
        (Reducer.Kind.Worse, this)
      } else {
        (Reducer.Kind.Same, other)
      }
    }
  }

  def noAbortAsMuchAsPossible: GenericNode => Reducer =
    (node: GenericNode) => NoAbortAsMushAsPossible(node)

  case class SelectOne(override val node: GenericNode, ambiguous: Boolean = false) extends Reducer {

    private val errorCount = Marker.countErrors(node.markers)

    override def compare(node: GenericNode): (Reducer.Kind.Kind, Reducer) = {
      val other = SelectOne(node)
      if (this.errorCount > 0) {
        if (other.errorCount < this.errorCount) {
          (Reducer.Kind.Better, other)
        } else {
          (Reducer.Kind.Worse, this)
        }
      } else {
        if (other.errorCount == 0) {
          (Reducer.Kind.Worse, SelectOne(this.node, ambiguous = true))
        } else
          (Reducer.Kind.Worse, this)
      }
    }

    override def close(subtrees: Seq[Subtree]): Seq[Subtree] =
      if (ambiguous) {
        // In case of ambiguity, exactly only one subtree is available
        val parsing = subtrees.head.stack.head
        Seq(Subtree(Seq(parsing.asAmbiguous())))
      } else
        subtrees
  }

  def selectOne: GenericNode => Reducer =
    (node: GenericNode) => SelectOne(node)

  type MarkerPostProcessor = GenericTree => Seq[Marker]
}

class Navigator(
  val result: Result,
  val postProcessors: Seq[Reducer.MarkerPostProcessor],
  val reducers: Seq[GenericNode => Reducer],
  private val userDataProvider: Option[UserDataProvider]
) {

  private val root: Subtrees =
    nonTerminal(result.successState, GlobalUserData(userDataProvider), successState = true)

  def hasNext: Boolean = root.choices.hasNext

  def next(): GenericTree = {
    val current = root.choices.next().stack.head
    var tree    =
      GenericTree(
        current.node,
        current.value,
        current.offset,
        current.length,
        current.markers ++ result.reportErrors()
      )
    if (postProcessors.nonEmpty) {
      var markers: Seq[Marker] = Seq.empty
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

  private def backPtrsOf(state: State): Seq[BackPtr] =
    result.contextOf(state).fold[Seq[BackPtr]](Seq.empty)(ctx => ctx.backPtrs.toSeq)

  private def sentinel(): Iterator[Subtree] =
    Seq(Subtree(Seq.empty)).iterator

  private def terminal(item: TerminalItem, userData: ContextualUserData): Iterator[Subtree] =
    singleton(applyToken(item, userData))

  private def singleton(value: Parsing): Iterator[Subtree] =
    Seq(Subtree(Seq(value))).iterator

  type AlternativeSupplier = ContextualUserData => Subtrees

  private def alternative(
    causal: AlternativeSupplier,
    predecessor: State,
    userData: ContextualUserData
  ): Iterator[Subtree] =
    new BackPtrIterator(causal, predecessor, userData)

  private def isContextual(state: State): Boolean = state.production.element match {
    case Some(element) => element match {
        case Bnf.DslSyntax(syntax) => syntax.contextual
        case _                     => false
      }
    case None          => false
  }

  private def nonTerminal(
    state: State,
    userData: ContextualUserData,
    successState: Boolean = false
  ): Subtrees = {
    val backPtrs: Seq[BackPtr] = backPtrsOf(state)
    if (backPtrs.isEmpty) {
      if (state.isCompleted)
        Subtrees(
          singleton(reduceState(state, Subtree(Seq.empty), userData, None)),
          userData
        )
      else {
        if (isContextual(state)) {
          Subtrees(sentinel(), userData.toLocalUserData())
        } else Subtrees(sentinel(), userData)
      }
    } else {
      val subtrees = backPtrs.map(backPtr =>
        alternative(
          backPtr.causal match {
            case item: TerminalItem => userData => Subtrees(terminal(item, userData), userData)
            case state: State       => userData => nonTerminal(state, userData)
          },
          backPtr.predecessor,
          userData
        )
      ).reduce(_ ++ _)
      if (state.isCompleted) {
        val candidates = subtrees.toSeq
        if (candidates.size > 1 && (successState || state.production.isDslElement)) {
          val ambiguity = Some(new Ambiguity(candidates.size))
          val subtrees  =
            candidates.map(s => Subtree(Seq(reduceState(state, s, userData, ambiguity))))
          Subtrees(filterSubtrees(subtrees, ambiguity).iterator, userData)
        } else
          Subtrees(
            candidates.map(s => Subtree(Seq(reduceState(state, s, userData, None)))).iterator,
            userData
          )
      } else
        Subtrees(subtrees, userData)
    }
  }

  private def filterSubtrees(
    candidates: Seq[Subtree],
    ambiguity: Option[Ambiguity]
  ): Seq[Subtree] = {
    var subtrees: Seq[Subtree] = candidates
    if (subtrees.nonEmpty) {
      val branchCount = subtrees.size
      subtrees = reducers.foldLeft(subtrees)((acc, r) => reduceSubtrees(acc, r))
      if (subtrees.size < branchCount) {
        ambiguity match {
          case Some(value) =>
            value.abort(branchCount - subtrees.size)
          case None        =>
        }
      }
    }
    subtrees
  }

  private def reduceSubtrees(
    subtrees: Seq[Subtree],
    reducer: GenericNode => Reducer
  ): Seq[Subtree] = {
    var reduced: Seq[Subtree] = Seq(subtrees.head)
    var ctx                   = reducer(reduced.head.stack.head.node)
    subtrees.tail.foreach(subtree => {
      val (kind, newCtx) = ctx.compare(subtree.stack.head.node)
      kind match {
        case Reducer.Kind.Better =>
          reduced = Seq(subtree)
        case Reducer.Kind.Same   =>
          reduced = reduced :+ subtree
        case Reducer.Kind.Worse  =>
      }
      ctx = newCtx
    })
    reduced = ctx.close(reduced)
    reduced
  }

  private def applyToken(terminal: TerminalItem, userData: ContextualUserData): Parsing = {
    val errors = terminal.reportErrors()
    val token  = terminal.token
    val node   = new GenericTerminal(
      new ParsingContext(
        userData,
        terminal.begin,
        terminal.end,
        token.offset,
        token.text.length,
        None,
        Seq.empty
      ),
      token
    )
    terminal match {
      case InsertedTokenValue(_, _, _) =>
        Parsing(node, terminal, token.offset, 0, errors)
      case _                           =>
        Parsing(node, terminal, token.offset, token.text.length, errors)
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
    userData: ContextualUserData,
    ambiguity: Option[Ambiguity],
    styles: Seq[(Token, Style)],
    errors: Seq[Marker]
  ): Parsing = {
    val context = new ParsingContext(
      userData,
      begin,
      end,
      offset,
      length,
      ambiguity,
      children
    )
    val value   = production.action(context, args)
    styles.foreach(pair => context.setTokenStyle(pair._1, pair._2))
    val markers = errors ++ context.markers
    Parsing(
      new GenericNonTerminal(context, markers, production, value),
      value,
      offset,
      length,
      markers
    )
  }

  private def reduceState(
    state: State,
    subtree: Subtree,
    userData: ContextualUserData,
    ambiguity: Option[Ambiguity]
  ): Parsing = {
    var children: Seq[GenericNode]  = IndexedSeq.empty
    var args: IndexedSeq[Any]       = IndexedSeq.empty
    var i                           = state.production.length
    var styles: Seq[(Token, Style)] = Seq.empty
    var errors: Seq[Marker]         = Seq.empty
    subtree.stack.foreach(arg => {
      if (i > 0) {
        arg.value match {
          case InsertedTokenValue(_, _, _) => /* Ignore */
          case item: TerminalItem          =>
            children = arg.node +: children
            args = item.token +: args
            item.style.foreach(s => styles = (item.token, s) +: styles)
            i -= 1
          case _                           =>
            children = arg.node +: children
            args = arg.value +: args
            i -= 1
        }
        errors = arg.markers ++ errors
      } else {
        arg.value match {
          case InsertedTokenValue(_, _, _) =>
            errors = arg.markers ++ errors
          case _                           =>
            throw new RuntimeException()
        }
      }
    })
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
      userData,
      ambiguity,
      styles,
      errors
    )
  }

  private class BackPtrIterator(
    val causalSupplier: AlternativeSupplier,
    val predecessor: State,
    val userData: ContextualUserData
  ) extends Iterator[Subtree] {

    private var predIterator = nonTerminal(predecessor, userData)
    private val causal       = causalSupplier.apply(predIterator.userData)
    private var current      =
      if (causal.choices.hasNext) causal.choices.next() else Subtree(Seq.empty)
    private var finished     = false

    override def next(): Subtree = {
      if (finished) {
        throw new NoSuchElementException()
      }
      val result: Subtree = Subtree(current.stack ++ predIterator.choices.next().stack)
      if (!predIterator.choices.hasNext) {
        if (causal.choices.hasNext) {
          current = causal.choices.next()
          predIterator = nonTerminal(predecessor, userData)
        } else
          finished = true
      }
      result
    }

    override def hasNext: Boolean = predIterator.choices.hasNext || causal.choices.hasNext
  }

  def expectOneTree(): Either[(String, Seq[GenericTree]), GenericTree] = {
    toIterator.toSeq match {
      case Nil          =>
        Left("No ASTs found !" -> Seq.empty)
      case head :: tail =>
        if (tail.isEmpty) {
          Right(head)
        } else {
          Left("Multiple ASTs found" -> (head +: tail))
        }
    }
  }
}
