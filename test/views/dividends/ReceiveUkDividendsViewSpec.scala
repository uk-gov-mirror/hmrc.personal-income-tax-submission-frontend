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

package views.dividends

import forms.YesNoForm
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import utils.ViewTest
import views.html.dividends.ReceiveUkDividendsView

class ReceiveUkDividendsViewSpec extends ViewTest {

  lazy val yesNoForm: Form[Boolean] = YesNoForm.yesNoForm("Select yes if dividends were received from the UK")

  lazy val receiveUkDividendsView: ReceiveUkDividendsView = app.injector.instanceOf[ReceiveUkDividendsView]

  val taxYear = 2020
  val expectedIndividualH1 = "Did you receive any dividends from companies in the UK?"
  val expectedIndividualTitle = "Did you receive any dividends from companies in the UK?"
  val expectedIndividualErrorTitle = s"Error: $expectedIndividualTitle"
  val expectedAgentH1 = "Did your client receive any dividends from companies in the UK?"
  val expectedAgentTitle = "Did your client receive any dividends from companies in the UK?"
  val expectedAgentErrorTitle = s"Error: $expectedAgentTitle"
  val captionText = "Dividends for 6 April 2019 to 5 April 2020"
  val yourDividendsText = "Your dividend voucher will usually show your shares in the company and the dividends received."
  val yesText = "Yes"
  val noText = "No"
  val continueText = "Continue"

  val captionSelector = ".govuk-caption-l"
  val yourDividendsSelector = "#value-hint"
  val yesSelector = "#main-content > div > div > form > div > fieldset > div.govuk-radios.govuk-radios--inline > div:nth-child(1) > label"
  val noSelector = "#main-content > div > div > form > div > fieldset > div.govuk-radios.govuk-radios--inline > div:nth-child(2) > label"
  val continueSelector = "#continue"

  "ReceivedUKDividendsView" should {

    "correctly render for an individual" when {

      "there are no form errors" which {

        lazy val view = receiveUkDividendsView(
          yesNoForm, taxYear)(user, implicitly, mockAppConfig)
        implicit lazy val document: Document = Jsoup.parse(view.body)

        titleCheck(expectedIndividualTitle)
        h1Check(expectedIndividualH1)
        textOnPageCheck(captionText, captionSelector)
        textOnPageCheck(yourDividendsText, yourDividendsSelector)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(yesText, yesSelector)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(noText, noSelector)
        buttonCheck(continueText, continueSelector)
      }

      "there is a form error due to no radio button selected" which {

        lazy val view = receiveUkDividendsView(
          yesNoForm.bind(Map("value" -> "")), taxYear)(user, implicitly, mockAppConfig)
        implicit lazy val document: Document = Jsoup.parse(view.body)

        val expectedErrorText = "Select yes if dividends were received from the UK"
        val errorSummaryHref = "#value"

        titleCheck(expectedIndividualErrorTitle)
        h1Check(expectedIndividualH1)
        textOnPageCheck(captionText, captionSelector)
        errorSummaryCheck(expectedErrorText, errorSummaryHref)
        textOnPageCheck(yourDividendsText, yourDividendsSelector)
        errorAboveElementCheck(expectedErrorText)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(yesText, yesSelector)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(noText, noSelector)
        buttonCheck(continueText, continueSelector)
      }
    }

    "correctly render for an agent" when {

      "there are no form errors" which {

        lazy val view = receiveUkDividendsView(
          yesNoForm, taxYear)(user.copy(arn = Some("XARN1234567")), implicitly, mockAppConfig)
        implicit lazy val document: Document = Jsoup.parse(view.body)

        titleCheck(expectedAgentTitle)
        h1Check(expectedAgentH1)
        textOnPageCheck(captionText, captionSelector)
        textOnPageCheck(yourDividendsText, yourDividendsSelector)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(yesText, yesSelector)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(noText, noSelector)
        buttonCheck(continueText, continueSelector)
      }

      "there is a form error due to no radio button selected" which {

        lazy val view = receiveUkDividendsView(
          yesNoForm.bind(Map("value" -> "")), taxYear)(user.copy(arn = Some("XARN1234567")), implicitly, mockAppConfig)
        implicit lazy val document: Document = Jsoup.parse(view.body)

        val expectedErrorText = "Select yes if dividends were received from the UK"
        val errorSummaryHref = "#value"

        titleCheck(expectedAgentErrorTitle)
        h1Check(expectedAgentH1)
        textOnPageCheck(captionText, captionSelector)
        errorSummaryCheck(expectedErrorText, errorSummaryHref)
        textOnPageCheck(yourDividendsText, yourDividendsSelector)
        errorAboveElementCheck(expectedErrorText)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(yesText, yesSelector)
//        TODO: Think of something for the radio buttons
        textOnPageCheck(noText, noSelector)
        buttonCheck(continueText, continueSelector)
      }
    }
  }
}
