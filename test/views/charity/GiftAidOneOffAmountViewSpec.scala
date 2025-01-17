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

package views.charity

import forms.charity.GiftAidOneOffAmountForm
import models.User
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import uk.gov.hmrc.auth.core.AffinityGroup
import utils.ViewTest
import views.html.charity.GiftAidOneOffAmountView

class GiftAidOneOffAmountViewSpec extends ViewTest {

  val taxYear = 2022
  def form(isAgent: Boolean): Form[BigDecimal] = GiftAidOneOffAmountForm.giftAidOneOffAmountForm(isAgent)
  lazy val view: GiftAidOneOffAmountView = app.injector.instanceOf[GiftAidOneOffAmountView]

  object IndividualExpected {
    val expectedTitle = "How much did you donate to charity as one-off payments?"
    val expectedH1 = "How much did you donate to charity as one-off payments?"
    val expectedParagraph = "Do not include the Gift Aid added to your donation."
    val expectedError = "Enter the amount you donated to charity as one-off payments"
  }

  object AgentExpected {
    val expectedTitle = "How much did your client donate to charity as one-off payments?"
    val expectedH1 = "How much did your client donate to charity as one-off payments?"
    val expectedParagraph = "Do not include the Gift Aid added to your client’s donation."
    val expectedError = "Enter the amount your client donated to charity as one-off payments"
  }

  val expectedCaption = "Charitable giving for 6 April 2021 to 5 April 2022"
  val expectedInputName = "amount"
  val expectedButtonText = "Continue"
  val expectedInputLabelText = "Total amount for the year, in pounds"
  val expectedInputHintText = "For example, £600 or £193.54"
  val expectedErrorLink = "#amount"

  val captionSelector = ".govuk-caption-l"
  val paragraphSelector = "#main-content > div > div > p"
  val inputFieldSelector = "#amount"
  val buttonSelector = ".govuk-button"
  val inputLabelSelector = ".govuk-label"
  val inputHintTextSelector = ".govuk-hint"

  "GiftAidOneOffAmount with no errors" when {

    "accessed as an individual" should {
      import IndividualExpected._

      implicit val user: User[_] = User("asdf", None, "AA123456A", AffinityGroup.Individual.toString)(fakeRequest)
      implicit lazy val document: Document = Jsoup.parse(view(taxYear, form(false), None).body)

      titleCheck(expectedTitle)
      h1Check(expectedH1)
      textOnPageCheck(expectedCaption, captionSelector)
      textOnPageCheck(expectedParagraph, paragraphSelector)
      textOnPageCheck(expectedInputLabelText, inputLabelSelector)
      textOnPageCheck(expectedInputHintText, inputHintTextSelector)
      inputFieldCheck(expectedInputName, inputFieldSelector)
      buttonCheck(expectedButtonText, buttonSelector)
    }

    "accessed as an agent" should {
      import AgentExpected._

      implicit val user: User[_] = User("asdf", Some("asdf"), "AA123456A", AffinityGroup.Agent.toString)(fakeRequest)
      implicit lazy val document: Document = Jsoup.parse(view(taxYear, form(true), None).body)

      titleCheck(expectedTitle)
      h1Check(expectedH1)
      textOnPageCheck(expectedCaption, captionSelector)
      textOnPageCheck(expectedParagraph, paragraphSelector)
      textOnPageCheck(expectedInputLabelText, inputLabelSelector)
      textOnPageCheck(expectedInputHintText, inputHintTextSelector)
      inputFieldCheck(expectedInputName, inputFieldSelector)
      buttonCheck(expectedButtonText, buttonSelector)
    }

  }

  "GiftAidOneOffAmount with errors" when {

    "accessed as an individual" should {
      import IndividualExpected._

      implicit val user: User[_] = User("asdf", None, "AA123456A", AffinityGroup.Individual.toString)(fakeRequest)
      implicit lazy val document: Document = Jsoup.parse(view(taxYear, form(false).bind(Map("amount" -> "")), None).body)

      titleCheck(expectedTitle)
      h1Check(expectedH1)
      textOnPageCheck(expectedCaption, captionSelector)
      textOnPageCheck(expectedParagraph, paragraphSelector)
      textOnPageCheck(expectedInputLabelText, inputLabelSelector)
      textOnPageCheck(expectedInputHintText, inputHintTextSelector)
      inputFieldCheck(expectedInputName, inputFieldSelector)
      buttonCheck(expectedButtonText, buttonSelector)

      errorSummaryCheck(expectedError, expectedErrorLink)
      errorAboveElementCheck(expectedError)
    }

    "accessed as an agent" should {
      import AgentExpected._

      implicit val user: User[_] = User("asdf", Some("asdf"), "AA123456A", AffinityGroup.Agent.toString)(fakeRequest)
      implicit lazy val document: Document = Jsoup.parse(view(taxYear, form(true).bind(Map("amount" -> "")), None).body)

      titleCheck(expectedTitle)
      h1Check(expectedH1)
      textOnPageCheck(expectedCaption, captionSelector)
      textOnPageCheck(expectedParagraph, paragraphSelector)
      textOnPageCheck(expectedInputLabelText, inputLabelSelector)
      textOnPageCheck(expectedInputHintText, inputHintTextSelector)
      inputFieldCheck(expectedInputName, inputFieldSelector)
      buttonCheck(expectedButtonText, buttonSelector)

      errorSummaryCheck(expectedError, expectedErrorLink)
      errorAboveElementCheck(expectedError)
    }

  }

}
