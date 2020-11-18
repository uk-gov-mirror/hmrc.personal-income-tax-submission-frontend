/*
 * Copyright 2020 HM Revenue & Customs
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

package forms.validation

import play.api.data.validation.{Constraint, Invalid, Valid}
import forms.validation.utils.ConstraintUtil._

object StringConstraints {

  val charRegex = """^([ A-Za-z0-9&@£$€¥#.,:;-])*$"""
  val intRegex = """^[0-9]*$"""

  val monetaryRegex = """\d+|\d*\.\d{1,2}"""

  val validateCurrency: String => Constraint[String] = msgKey => constraint[String](
    value => if(value.matches(monetaryRegex)) Valid else Invalid(msgKey)
  )

  val validateInt: String => Constraint[String] = msgKey => constraint[String](
    x => if (x.matches(intRegex)) Valid else Invalid(msgKey)
  )

  val validateChar: String => Constraint[String] = msgKey => constraint[String](
  x => if (x.matches(charRegex)) Valid else Invalid(msgKey)
  )

  val nonEmpty: String => Constraint[String] = msgKey => constraint[String](
    x => if (x.isEmpty) Invalid(msgKey) else Valid
  )

  val maxLength: (Int, String) => Constraint[String] = (length, msgKey) => constraint[String](
    x => if (x.trim.length > length) Invalid(msgKey) else Valid
  )

  val noLeadingSpace: String => Constraint[String] = msgKey => constraint[String](
    x => if (x.headOption.contains(" ".head)) Invalid(msgKey) else Valid
  )

}
