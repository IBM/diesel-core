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

package diesel.voc

import java.io.Writer
import java.util.StringTokenizer
import scala.collection.mutable
import scala.util.Try

object SentenceProcessor {

  trait Handler {
    def processSentence(sentence: Sentence, context: VerbalizationContext): Unit = {}
    def processSyntacticRole(syntacticRole: SyntacticRole, context: VerbalizationContext): Unit
    def processText(text: String): Unit
  }

  trait HandlerExtension {
    def processPartitiveArticle(text: String): Unit
  }

  abstract class WriterHandler(roles: Seq[Role], val writer: Writer) extends Handler {

    protected def getRole(index: Int): Role = roles.find(_.index == index).get

    override def processSyntacticRole(
      syntacticRole: SyntacticRole,
      context: VerbalizationContext
    ): Unit = {
      writer.write(getRole(syntacticRole.roleIndex).label.getOrElse("??"))
    }

    override def processText(text: String): Unit = {
      writer.write(text)
    }
  }

  abstract class AbstractVerbalizerSentenceHandler(
    roles: Seq[Role],
    writer: Writer,
    val verbalizer: Verbalizer
  ) extends WriterHandler(roles, writer) {
    override def processSyntacticRole(
      synRole: SyntacticRole,
      context: VerbalizationContext
    ): Unit = {
      synRole.category match {
        case Object  =>
          processObject(synRole, context)
        case Subject =>
          processSubject(synRole, context)
      }
    }

    def processSubject(role: SyntacticRole, context: VerbalizationContext): Unit
    def processObject(role: SyntacticRole, context: VerbalizationContext): Unit
  }

  class DefaultVerbalizerSentenceHandler(
    roles: Seq[Role],
    writer: Writer,
    verbalizer: Verbalizer,
    private val useBracket: Boolean
  ) extends AbstractVerbalizerSentenceHandler(roles, writer, verbalizer) {

    private var sentence: Option[Sentence] = None

    override def processSentence(sentence: Sentence, context: VerbalizationContext): Unit = {
      super.processSentence(sentence, context)
      this.sentence = Some(sentence)
    }

    override def processSubject(role: SyntacticRole, context: VerbalizationContext): Unit = {
      sentence.foreach { s =>
        val template     = s.template
        var subjectFirst = false
        val open         = 1 + template.indexOf('{')
        if open > 0 then {
          val close = template.indexOf('}', open)
          if close >= 0 then {
            val comma = template.indexOf(',', open)
            val index = template.substring(
              open,
              if comma < 0 then {
                close
              } else {
                Math.min(comma, close)
              }
            )
            Try(index.toInt)
              .toOption
              .foreach { idx =>
                if idx > 0 then {
                  subjectFirst = true
                }
              }
          }
        }
        var ctx          = context
        if subjectFirst then {
          ctx = ctx.copy(article = DefiniteArticle)
        } else {
          ctx = ctx.copy(article = NoArticle)
        }
        if role.cardinality == Multiple then {
          ctx = ctx.copy(plural = true)
        }
        if useBracket then {
          writeOpenBracket(true)
        }
        writer.write(verbalizer.verbalizeTerm(ctx, RoleVerbalizable(getRole(role.roleIndex))).text)
        if useBracket then {
          writeCloseBracket(true)
        }
      }
    }

    protected def writeOpenBracket(subject: Boolean): Unit = {
      writer.write("{")
    }

    protected def writeCloseBracket(subject: Boolean): Unit = {
      writer.write("}")
    }

    override def processObject(role: SyntacticRole, context: VerbalizationContext): Unit = {
      val label = getRole(role.roleIndex).label
      if useBracket then writeOpenBracket(false)
      writeObjectText(label.getOrElse("?"))
      if useBracket then writeCloseBracket(false)
    }

    protected def writeObjectText(s: String): Unit = {
      writer.write(s)
    }
  }

  class BusinessVerbalizerSentenceHandler(roles: Seq[Role], writer: Writer, verbalizer: Verbalizer)
      extends DefaultVerbalizerSentenceHandler(roles, writer, verbalizer, false) {

    override def processObject(synRole: SyntacticRole, context: VerbalizationContext): Unit = {
      val c = context.copy(
        article = IndefiniteArticle,
        plural = synRole.cardinality == Multiple
      )
      val s = verbalizer.verbalizeTerm(c, RoleVerbalizable(getRole(synRole.roleIndex)))
      writer.write("<")
      writer.write(s.text)
      writer.write(">")
    }
  }

//  class BusinessVerbalizerSentenceHandler(writer: Writer, verbalizer: Verbalizer, voc: Vocabulary)

  private val PLACE_HOLDER_OPEN: Char            = '{'
  private val PLACE_HOLDER_CLOSE: Char           = '}'
  private val PLACE_HOLDER_PROPS_SEPARATOR: Char = ','

  private def mustGeneratePartitiveArticle(
    context: VerbalizationContext,
    propagatePartitif: Boolean
  ): Boolean =
    context.partitive && propagatePartitif

  private def generatePartitiveArticle(
    verbalizer: Verbalizer,
    context: VerbalizationContext,
    handler: Handler
  ): Unit = {
    // TODO make the same kind of verbalization trick for grammatical construction
    // like sequence (and text on choice or element) in the grammar
    // INFO By construction partitive article is prepend
    val local   = context.copy(
      partitive = true,
      article = NoArticle
    )
    val article = verbalizer.articleBuilder.getArticle(local, UnspecifiedVerbalizable())
    article.foreach { art =>
      handler match {
        case extension: HandlerExtension =>
          extension.processPartitiveArticle(art)
        case _                           =>
          handler.processText(art)
      }
    }
  }

  def processSentence(
    verbalizer: Verbalizer,
    sentence: Sentence,
    context: VerbalizationContext,
    handler: Handler
  ): Unit = {
    var ctx                = context
    val textBuffer         = new StringBuilder()
    val template           = sentence.template
    if template.isEmpty then {
      return
    }
    val synRoleBuffer      = new StringBuilder()
    val synRolePropsBuffer = new StringBuilder()
    var index              = 0
    var isInSynRole        = false
    var isInSynRoleProps   = false
    ctx = ctx.copy(sentence = Some(sentence))
    handler.processSentence(sentence, ctx)
    var propagatePartitif  = true
    for c <- template do {
      c match {
        case PLACE_HOLDER_OPEN                 =>
          isInSynRole = true
          synRoleBuffer.clear()
          synRolePropsBuffer.clear()
          if textBuffer.nonEmpty then {
            if mustGeneratePartitiveArticle(ctx, propagatePartitif) then {
              generatePartitiveArticle(verbalizer, ctx, handler)
            }
            handler.processText(textBuffer.substring(0, textBuffer.length))
            propagatePartitif = false
            textBuffer.setLength(0)
          }
        case PLACE_HOLDER_CLOSE if isInSynRole =>
          val synRoleAsString                   = synRoleBuffer.toString()
          val synRoleOpt: Option[SyntacticRole] = Try(
            synRoleAsString.toInt
          ).toOption.flatMap(synRoleIndex => sentence.getSyntacticRole(synRoleIndex))
          synRoleOpt match {
            case Some(synRole) =>
              if synRolePropsBuffer.nonEmpty then {
                val props     = synRolePropsBuffer.toString.trim
                val propsList = getSyntacticRoleProperties(props)
                if propsList.nonEmpty then {
                  ctx = ctx.copy(props = propsList)
                }
              }
              val ctxLocal = buildLocalContextState(ctx, synRole, propagatePartitif)
              handler.processSyntacticRole(synRole, ctxLocal)
              propagatePartitif = false
              isInSynRole = false
              isInSynRoleProps = false

            case None =>
              // Unparseable place holder. Let's handle it as a text:
              if mustGeneratePartitiveArticle(ctx, propagatePartitif) then {
                generatePartitiveArticle(verbalizer, ctx, handler)
              }
              handler.processText(s"$PLACE_HOLDER_OPEN$synRoleAsString$PLACE_HOLDER_CLOSE")
              propagatePartitif = false
          }
        case _                                 =>
          if isInSynRole then {
            if isInSynRoleProps then {
              synRolePropsBuffer.append(c)
            } else {
              if c == PLACE_HOLDER_PROPS_SEPARATOR then {
                isInSynRoleProps = true
              } else {
                synRoleBuffer.append(c)
              }
            }
          } else {
            textBuffer.append(c)
          }
      }
    }
    if textBuffer.nonEmpty then {
      if mustGeneratePartitiveArticle(ctx, propagatePartitif) then {
        generatePartitiveArticle(verbalizer, ctx, handler)
      }
      handler.processText(textBuffer.toString())
    }
  }

  private def getSyntacticRoleProperties(props: String): Map[String, String] = {
    val tokenizer = new StringTokenizer(props, ", ")
    val propsMap  = mutable.Map[String, String]()
    while {
      tokenizer.hasMoreTokens
    } do {
      val prop  = tokenizer.nextToken
      val index = prop.indexOf('=')
      if index == -1 then propsMap.put(prop, "true")
      else {
        val key   = prop.substring(0, index)
        val `val` = prop.substring(index + 1)
        propsMap.put(key, `val`)
      }
    }
    propsMap.toMap
  }

  private def buildLocalContextState(
    context: VerbalizationContext,
    synRole: SyntacticRole,
    propagatePartitif: Boolean
  ): VerbalizationContext = {
    var ctx = context.getProperty(Vocabulary.Constants.PARTITIVE_ARTICLE) match {
      case Some(value) =>
        context.copy(partitive = value.equalsIgnoreCase("TRUE"))
      case None        =>
        context.copy(partitive = context.partitive && propagatePartitif)
    }
    if context.getProperty(Vocabulary.Constants.NO_ARTICLE).exists(_.equalsIgnoreCase("TRUE"))
    then {
      ctx = ctx.copy(article = NoArticle)
    }
    if
      context.getProperty(Vocabulary.Constants.PLURAL_UNDEFINED).exists(_.equalsIgnoreCase("TRUE"))
    then {
      ctx = ctx.copy(plural = synRole.cardinality == Multiple)
    }
    ctx
  }
}
