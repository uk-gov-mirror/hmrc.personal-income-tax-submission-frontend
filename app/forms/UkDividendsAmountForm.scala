/*
 * Copyright 2021 HM Revenue & Customs
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

package forms

import forms.validation.mappings.MappingUtil._
import models.User
import play.api.data.Form


object UkDividendsAmountForm {

  val ukDividendsAmount: String = "amount"

  def ukDividendsAmountForm(implicit user: User[_]): Form[BigDecimal] = Form(
    ukDividendsAmount -> currency(
      s"dividends.uk-dividends-amount.error.empty.${if(user.isAgent) "agent" else "individual"}",
      invalidNumeric = s"dividends.uk-dividends-amount.error.invalidFormat.${if(user.isAgent) "agent" else "individual"}",
      nonNumericKey = s"dividends.uk-dividends-amount.error.invalidFormat.${if(user.isAgent) "agent" else "individual"}",
      maxAmountKey = s"dividends.uk-dividends-amount.error.amountMaxLimit")
  )
}

