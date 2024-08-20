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

import diesel.i18n.Messages.{Lang, Resolver}
import diesel.i18n.{Lang, Loader, MessageFormat, Messages}

object DieselI18n extends Messages {

  val missingToken: Msg1[String]          = msg1
  val insertedToken: Msg1[String]         = msg1
  val tokenMutation: Msg2[String, String] = msg2
  val unknownToken: Msg1[String]          = msg1
  val ambiguous: Msg0                     = msg0
  val incompatible: Msg0                  = msg0

  override protected def load(): Map[Lang, Map[String, MessageFormat]] =
    I18nFiles.messages
      .flatMap { case (k, v) =>
        Lang(k).flatMap(lang => Loader.loadProperties(v).map((lang, _)))
      }
      .toMap

  override protected def newResolver(loaded: Map[Lang, Map[String, MessageFormat]])
    : Messages.Resolver =
    Resolver(loaded).withFallback(Lang.EN)

}
