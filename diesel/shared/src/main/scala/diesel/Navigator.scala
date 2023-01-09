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

import diesel.Analyzer.Ambiguity
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
  val navigator: Navigator,
  val children: Seq[GenericNode]
) extends Context {

  override def getUserData(key: Any): Option[Any] = userDataProvider.flatMap(_.getUserData(key))

  override def setUserData(key: Any, value: Any): Unit =
    userDataProvider.foreach(_.setUserData(key, value))

  private var locals: Seq[Marker]              = Seq()
  private var style: Option[Style]             = None
  private var tokenStyles: Seq[(Token, Style)] = Seq()
  private var aborted: Boolean                 = false

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
  private[diesel] def toString(buf: StringBuilder): Unit

  def offset: Int = context.offset

  def length: Int = context.length

  def valueAs[T]: T = value.asInstanceOf[T]

  def hasAmbiguity: Boolean = false

  override def toString: String = {
    val buf: StringBuilder = new StringBuilder
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
      new ParsingContext(None, -1, -1, 0, -1, None, Seq(), null, Seq()),
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

}

class Navigator(
  val result: Result,
  val postProcessors: Seq[GenericTree => Seq[Marker]],
  private val userDataProvider: Option[UserDataProvider]
) {

  import Analyzer._

  private var choices: Seq[Choice]        = Seq()
  private[diesel] var stack: Seq[Parsing] = Seq()
  private var root: Root                  = _
  private var proc: Process               = _
  private var current: Parsing            = _

  private object Sentinel

  moveToFirst

  private[diesel] def choice(statement: Statement, state: State): Process = {
    if (state.production.length == 0) {
      pushSentinel()
      statement.next()
    } else {
      val choice = new Choice(this, statement, state)
      choices = Seq[Choice](choice) ++ choices
      choice
    }
  }

  private[diesel] def unary(
    statement: Statement,
    item: Item,
    ambiguity: Option[Ambiguity]
  ): Statement = {
    new Unary(this, statement, item, ambiguity)
  }

  private[diesel] def push(item: Item, ambiguity: Option[Ambiguity]): Boolean = {
    item match {
      case terminal: TerminalItem =>
        def value: Parsing = applyToken(terminal)
        stack = Seq(value) ++ stack
        true

      case state: State =>
        reduce(state, ambiguity)
    }
  }

  private[diesel] def tree(state: State): Boolean = {
    push(state, None)
    val top       = stack.head
    val candidate =
      Parsing(top.node, top.value, top.offset, top.length, top.markers ++ result.reportErrors())
    current = candidate
    true
  }

  private[diesel] def pushSentinel(): Unit = {
    stack = Seq(Parsing(GenericSentinel, Sentinel, -1, 0, Seq())) ++ stack
  }

  private[diesel] def reduce(state: State, ambiguity: Option[Ambiguity]): Boolean = {
    var children: Seq[GenericNode]  = IndexedSeq()
    var args: IndexedSeq[Any]       = IndexedSeq()
    var i                           = state.production.length
    var styles: Seq[(Token, Style)] = Seq()
    var errors: Seq[Marker]         = Seq()
    while (i > 0) {
      val value = stack.head
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
      stack = stack.tail
    }
    errors = popSentinel(errors)
    val offset                      = result.tokenAt(state.begin).map(_.offset).getOrElse(-1)
    val length                      =
      if (state.begin == state.end) {
        0
      } else {
        result.tokenAt(state.end - 1).map(tk => tk.offset + tk.text.length).getOrElse(-1) - offset
      }
    val parsingCtx                  =
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
    stack = Seq(parsingCtx) ++ stack
    !parsingCtx.node.context.hasAborted
  }

  private def popSentinel(errors: Seq[Marker]): Seq[Marker] = {
    var res = errors
    // Pop all remaining inserted token values
    while (stack.head.value != Sentinel) {
      val value = stack.head
      value.value match {
        case InsertedTokenValue(_, _, _) =>
          res = value.markers ++ res

        case _ =>
          throw new RuntimeException()
      }
      stack = stack.tail
    }
    stack = stack.tail // Pop sentinel
    res
  }

  private def computeErrors(state: State, stack: Seq[Parsing]): Seq[Marker] = {
    var i                   = state.production.length
    var cursor              = stack;
    var errors: Seq[Marker] = Seq()
    while (i > 0) {
      val value = cursor.head
      value.value match {
        case InsertedTokenValue(_, _, _) => /* Ignore */
        case _                           =>
          i -= 1
      }
      errors = value.markers ++ errors
      cursor = cursor.tail
    }
    // Pop all remaining inserted token values
    while (cursor.head.value != Sentinel) {
      val value = cursor.head
      value.value match {
        case InsertedTokenValue(_, _, _) =>
          errors = value.markers ++ errors
        case _                           =>
          throw new RuntimeException()
      }
      cursor = cursor.tail
    }
    errors
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
      this,
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
        this,
        Seq()
      ),
      token
    )
    terminal match {
      case InsertedTokenValue(_, _, _) =>
        Parsing(node, terminal, token.offset, 0, errors)

      case _ =>
        Parsing(node, terminal, token.offset, token.text.length, terminal.reportErrors())
    }
  }

  private def moveToFirst: Boolean = moveToFirst(result.successState)

  private def moveToFirst(state: State): Boolean = {
    stack = Seq()
    root = new Root(this, state)
    proc = root
    moveToNext
  }

  private def moveToNext: Boolean = {
    while (!proc.done) {
      proc = proc.step()
      if (proc.pause)
        return true
      if (proc.stop || (proc.cut && canRetry))
        proc = back()
    }
    current = null
    root.succeeded
  }

  def hasNext: Boolean = current != null

  def next(): GenericTree = {
    var tree =
      GenericTree(current.node, current.value, current.offset, current.length, current.markers)
    if (postProcessors.nonEmpty) {
      var markers: Seq[Marker] = Seq()
      postProcessors.foreach(pp => markers = markers ++ pp(tree))
      tree = GenericTree(tree.root, tree.value, tree.offset, tree.length, tree.markers ++ markers)
    }
    moveToNext
    tree
  }

  def toIterator: Iterator[GenericTree] with Object {} = {
    val nav: Navigator = this
    new Iterator[GenericTree] {
      override def hasNext: Boolean = nav.hasNext

      override def next(): GenericTree = nav.next()
    }
  }

  private def back(): Process = {
    while (choices.nonEmpty) {
      val choice = choices.head
      if (choice.retry) {
        stack = choice.base
        return choice
      }
      choices = choices.tail
    }
    new Stop(this)
  }

  private def canRetry: Boolean = {
    if (choices.nonEmpty) choices.exists(_.retry) else false
  }

  private[diesel] def selectSubtrees(state: State, trees: Seq[Seq[Parsing]]): Seq[Seq[Parsing]] = {
    var results: Seq[Seq[Parsing]] = Seq()
    var cursor                     = trees
    var errorCount                 = Int.MaxValue;
    while (cursor.nonEmpty) {
      val localErrorCount = Marker.countErrors(computeErrors(state, cursor.head))
      if (localErrorCount < errorCount) {
        results = Seq(cursor.head)
        errorCount = localErrorCount;
      } else if (localErrorCount == errorCount) {
        results = results ++ Seq(cursor.head)
      }
      cursor = cursor.tail
    }
    results
  }
}

object Analyzer {

  private[diesel] trait Process {
    def navigator: Navigator

    def step(): Process

    def pause: Boolean = false

    def stop: Boolean = false

    def cut: Boolean = false

    def done: Boolean = stop || cut
  }

  private[diesel] abstract class Statement(val navigator: Navigator, val parent: Statement)
      extends Process {

    def next(): Statement = parent.next()
  }

  private[diesel] class Unary(
    override val navigator: Navigator,
    override val parent: Statement,
    val item: Item,
    val ambiguity: Option[Ambiguity]
  ) extends Statement(navigator, parent) {

    override def step(): Process = {
      item match {
        case _: TerminalItem =>
          next()

        case state: State =>
          navigator.choice(this, state)
      }
    }

    override def next(): Statement = {
      if (!navigator.push(item, ambiguity)) {
        return new Cut(navigator)
      }
      super.next();
    }
  }

  private[diesel] class Sequence(
    override val navigator: Navigator,
    override val parent: Statement,
    val state: State,
    val backPtr: BackPtr,
    val ambiguity: Option[Ambiguity]
  ) extends Statement(navigator, parent) {

    override def step(): Process = {
      if (
        backPtr.predecessor.dot == 0 && navigator.result.contextOf(backPtr.predecessor).forall(
          ctx => ctx.backPtrs.isEmpty
        )
      ) {
        navigator.pushSentinel()
        next()
      } else {
        navigator.choice(this, backPtr.predecessor)
      }
    }

    override def next(): Statement = {
      navigator.unary(parent, backPtr.causal, ambiguity)
    }
  }

  private[diesel] class Ambiguity(val branchCount: Int) {

    private var abortedBranchCount: Int = 0

    private[diesel] def abort(): Unit = abortedBranchCount += 1

    private[diesel] def ambiguous: Boolean = branchCount - abortedBranchCount > 1
  }

  private[diesel] class Subtree(
    val choice: Choice,
    val state: State,
    val backPtr: BackPtr,
    val ambiguity: Option[Ambiguity]
  ) extends Statement(choice.navigator, choice.parent) {

    override def step(): Process = new Sequence(navigator, this, state, backPtr, ambiguity)

    override def next(): Statement = {
      choice.addSubtree()
      new Stop(navigator)
    }
  }

  private[diesel] class Propose(val choice: Choice, val stack: Seq[Parsing])
      extends Statement(choice.navigator, choice.parent) {

    override def step(): Process = {
      navigator.stack = stack
      next()
    }
  }

  private[diesel] class Choice(val navigator: Navigator, val parent: Statement, val state: State)
      extends Process {

    private val backPtrs: Seq[BackPtr]       =
      navigator.result.contextOf(state).fold[Seq[BackPtr]](Seq())(ctx => ctx.backPtrs.toSeq)
    private[diesel] val base: Seq[Parsing]   = navigator.stack
    private var index: Seq[BackPtr]          = backPtrs
    private val ambiguity: Option[Ambiguity] =
      if (backPtrs.size > 1) Some(new Ambiguity(backPtrs.size)) else None
    private var subtrees: Seq[Seq[Parsing]]  = Seq()

    override def step(): Process = {
      if (state.isCompleted) {
        if (index.nonEmpty) {
          val backPtr = index.head
          index = index.tail
          return new Subtree(this, state, backPtr, ambiguity)
        } else {
          if (subtrees.nonEmpty) {
            val subtree = subtrees.head
            subtrees = subtrees.tail
            return new Propose(this, subtree)
          }
        }
      } else {
        if (index.nonEmpty) {
          val backPtr = index.head
          index = index.tail
          return new Sequence(navigator, parent, state, backPtr, ambiguity)
        }
      }
      new Stop(navigator)
    }

    def addSubtree(): Unit = {
      subtrees = subtrees ++ Seq(navigator.stack)
      if (index.isEmpty && subtrees.size > 1) {
        subtrees = navigator.selectSubtrees(state, subtrees)
      }
    }

    def retry: Boolean = index.nonEmpty || subtrees.nonEmpty
  }

  private[diesel] class Tree(override val navigator: Navigator, val root: Root)
      extends Statement(navigator, root) {

    override def step(): Process = navigator.choice(this, root.state)

    override def next(): Statement = {
      if (navigator.tree(root.state))
        new Pause(navigator, root)
      else
        super.next()
    }
  }

  private[diesel] class Root(override val navigator: Navigator, val state: State)
      extends Statement(navigator, new Stop(navigator)) {

    private var succeed = false

    override def step(): Process = new Tree(navigator, this)

    override def next(): Statement = {
      succeed = true
      new Stop(navigator)
    }

    def succeeded: Boolean = succeed
  }

  private[diesel] class Pause(override val navigator: Navigator, override val parent: Statement)
      extends Statement(navigator, parent) {

    override def step(): Process = next()

    override def pause: Boolean = true
  }

  private[diesel] class Stop(override val navigator: Navigator) extends Statement(navigator, null) {

    override def step(): Process = {
      throw new NotImplementedError()
    }

    override def next(): Statement = {
      throw new NotImplementedError()
    }

    override def stop: Boolean = true
  }

  private[diesel] class Cut(override val navigator: Navigator) extends Statement(navigator, null) {

    override def step(): Process = {
      throw new NotImplementedError()
    }

    override def next(): Statement = {
      throw new NotImplementedError()
    }

    override def cut: Boolean = true
  }

  //  case class StyleRange[S](val offset: )
}
