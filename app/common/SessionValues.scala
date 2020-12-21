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

package common

object SessionValues {
  val CLIENT_MTDITID = "MTDITID"
  val CLIENT_NINO = "NINO"

  val DIVIDENDS_CYA = "DIVIDENDS_CYA"
  val DIVIDENDS_PRIOR_SUB = "DIVIDENDS_PRIOR_SUB"
  val INTEREST_PRIOR_SUB = "INTEREST_PRIOR_SUB"

  val INTEREST_CYA = "INTEREST_CYA"

  val PAGE_BACK_UNTAXED_AMOUNT = "SASS_PAGE_BACK_UNTAXED_AMOUNT"
  val PAGE_BACK_TAXED_AMOUNT = "SASS_PAGE_BACK_TAXED_AMOUNT"

  val PAGE_BACK_UNTAXED_ACCOUNTS = "SASS_PAGE_BACK_UNTAXED_ACCOUNTS"
  val PAGE_BACK_TAXED_ACCOUNTS = "SASS_PAGE_BACK_TAXED_ACCOUNTS"

  val PAGE_BACK_CYA = "SASS_PAGE_BACK_CYA"
}
