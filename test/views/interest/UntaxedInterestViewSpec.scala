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

package views.interest

import forms.YesNoForm

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.{Form, FormError}
import utils.ViewTest
import views.html.interest.UntaxedInterestView

class UntaxedInterestViewSpec extends ViewTest {


  lazy val yesNoForm: Form[Boolean] = YesNoForm.yesNoForm("Select yes if untaxed interest " +
    "was received from companies in the uk")

  lazy val untaxedInterestView: UntaxedInterestView = app.injector.instanceOf[UntaxedInterestView]

  val h1Selector = "h1"
  val captionSelector = ".govuk-caption-l"
  val yesOptionSelector = "#value"
  val noOptionSelector = "#value-no"
  val continueButtonSelector = "#continue"

  val errorSummarySelector = ".govuk-error-summary"
  val errorSummaryTitleSelector = ".govuk-error-summary__title"
  val errorSummaryTextSelector = ".govuk-error-summary__body"

  val expectedIndividualH1 = "Did you receive any untaxed interest from the UK?"
  val expectedIndividualTitle = "Did you receive any untaxed interest from the UK?"
  val expectedIndividualErrorTitle = s"Error: $expectedIndividualTitle"
  val expectedAgentH1 = "Did your client receive any untaxed interest from the UK?"
  val expectedAgentTitle = "Did your client receive any untaxed interest from the UK?"
  val expectedAgentErrorTitle = s"Error: $expectedAgentTitle"

  val expectedCaption = "Interest for 06 April 2019 to 05 April 2020"

  val expectedErrorSummaryTitle = "There is a problem"
  val expectedErrorSummaryText = "Select yes if untaxed interest was received from companies in the UK"

  val taxYear = 2020

  "ReceivedUntaxedInterestView" should {

    "correctly render with no errors as an individual" when {

      "there are no form errors" which {

        lazy val view = untaxedInterestView(
          yesNoForm, taxYear)(user,implicitly,mockAppConfig)

        implicit lazy val document: Document = Jsoup.parse(view.body)

        titleCheck(expectedIndividualTitle)

        "contains the correct h1" in {
          elementText(h1Selector) shouldBe expectedIndividualH1
        }

        "contains the correct caption" in {
          elementText(captionSelector) shouldBe expectedCaption
        }

        "contains a yes option" in {
          elementExist(yesOptionSelector) shouldBe true
        }

        "contains a no option" in {
          elementExist(noOptionSelector) shouldBe true
        }

        "contains a continue button" in {
          elementExist(continueButtonSelector) shouldBe true
        }
      }
    }

    "correctly render with errors as an individual"  when {

      "there are form errors" which {

        lazy val view = untaxedInterestView(
          yesNoForm.copy(
          errors = Seq(FormError("yes_no", "Select yes if untaxed interest was received from companies in the UK"))),
          2020)(user,implicitly,mockAppConfig)

        implicit lazy val document: Document = Jsoup.parse(view.body)

        titleCheck(expectedIndividualErrorTitle)

        "contains the correct h1" in {
          elementText(h1Selector) shouldBe expectedIndividualH1
        }

        "contains the correct header caption" in {
          elementText(captionSelector) shouldBe expectedCaption
        }

        "contains a yes option" in {
          elementExist(yesOptionSelector) shouldBe true
        }

        "contains a no option" in {
          elementExist(noOptionSelector) shouldBe true
        }

        "contains a continue button" in {
          elementExist(continueButtonSelector) shouldBe true
        }

        "contains an error" in {
          elementExist(errorSummarySelector) shouldBe true
        }

        "contains an error title" in {
          elementText(errorSummaryTitleSelector) shouldBe expectedErrorSummaryTitle
        }

        "contains an error message" in {
          elementText(errorSummaryTextSelector) shouldBe expectedErrorSummaryText
        }
      }
    }

    "correctly renders with no errors as an agent" when {

      "there are no form errors" which {

        lazy val view = untaxedInterestView(
          yesNoForm,taxYear)(user.copy(arn = Some("XARN1234567")),implicitly,mockAppConfig)

        implicit lazy val document: Document = Jsoup.parse(view.body)

        titleCheck(expectedAgentTitle)

        "contain the correct h1" in {
          elementText(h1Selector) shouldBe expectedAgentH1
        }

        "contains the correct header caption" in {
          elementText(captionSelector) shouldBe expectedCaption
        }

        "contains a yes option" in {
          elementExist(yesOptionSelector) shouldBe true
        }

        "contains a no option" in {
          elementExist(noOptionSelector) shouldBe true
        }

        "contains a continue button" in {
          elementExist(continueButtonSelector) shouldBe true
        }
      }
    }

    "correctly render with errors as an agent" when {

      "there is a form error" which {

        lazy val view = untaxedInterestView(
          yesNoForm.copy(
            errors = Seq(FormError("yes_no", "Select yes if untaxed interest was received from companies in the UK"))),
          taxYear)(user.copy(arn = Some("XARN1234567")),implicitly,mockAppConfig)

        implicit lazy val document: Document = Jsoup.parse(view.body)

        titleCheck(expectedAgentErrorTitle)

        "contain the correct h1" in {
          elementText(h1Selector) shouldBe expectedAgentH1
        }

        "contains the correct header caption" in {
          elementText(captionSelector) shouldBe expectedCaption
        }

        "contains a yes option" in {
          elementExist(yesOptionSelector) shouldBe true
        }

        "contains a no option" in {
          elementExist(noOptionSelector) shouldBe true
        }

        "contains a continue button" in {
          elementExist(continueButtonSelector) shouldBe true
        }

        "contains an error" in {
          elementExist(errorSummarySelector) shouldBe true
        }

        "contain an error title" in {
          elementText(errorSummaryTitleSelector) shouldBe expectedErrorSummaryTitle
        }

        "contains an error message" in {
          elementText(errorSummaryTextSelector) shouldBe expectedErrorSummaryText
        }
      }
    }
  }
}
