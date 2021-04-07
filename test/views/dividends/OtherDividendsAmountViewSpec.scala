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

import forms.OtherDividendsAmountForm
import models.DividendsPriorSubmission
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.data.Form
import utils.ViewTest
import views.html.dividends.OtherUkDividendsAmountView

class OtherDividendsAmountViewSpec extends ViewTest {

  val priorAmount = 20

  lazy val otherDividendsAmountForm: Form[BigDecimal] = OtherDividendsAmountForm.otherDividendsAmountForm()
  lazy val otherDividendsAmountView: OtherUkDividendsAmountView = app.injector.instanceOf[OtherUkDividendsAmountView]

  val taxYear: Int = 2020
  val taxYearMinusOne: Int = taxYear - 1

  val poundPrefixSelector = ".govuk-input__prefix"
  val captionSelector = ".govuk-caption-l"
  val inputSelector = ".govuk-input"
  val continueButtonSelector = "#continue"
  val continueButtonFormSelector = "#main-content > div > div > form"
  val enterAmountSelector = "#amount"
  val youToldUsSelector = "#main-content > div > div > form > div > label"
  val tellUsTheValueSelector = "#main-content > div > div > form > div > label"

  val expectedH1 = "How much did you get in dividends from UK-based trusts and open-ended investment companies?"
  val expectedH1Agent = "How much did your client get in dividends from UK-based trusts and open-ended investment companies?"
  val expectedTitle = "How much did you get in dividends from UK-based trusts and open-ended investment companies?"
  val expectedTitleAgent = "How much did your client get in dividends from UK-based trusts and open-ended investment companies?"
  val expectedErrorTitle = s"Error: $expectedTitle"
  val expectedErrorTitleAgent = s"Error: $expectedTitleAgent"
  val expectedCaption = s"Dividends for 6 April $taxYearMinusOne to 5 April $taxYear"
  val tellUsTheValueIndividual = "Tell us the value of the dividends you got, in pounds. You can find this information in your dividend voucher."
  val tellUsTheValueAgent = "Tell us the value of the dividends your client got, in pounds. You can find this information in their dividend voucher."

  def youToldUsPriorTextIndividual(amount: String): String =
    s"You told us you got £$amount in dividends from UK-based trusts and open-ended investment companies this year. Tell us if this has changed."
  def youToldUsPriorTextAgent(amount: String): String =
    s"You told us your client got £$amount in dividends from UK-based trusts and open-ended investment companies this year. Tell us if this has changed."

  val expectedHintText = "For example, £600 or £193.54"
  val poundPrefixText = "£"
  val differentAmountText = "A different amount"
  val continueText = "Continue"
  val continueLink = "/test-url"
  val newAmountInput = "#amount"
  val amountInputName = "amount"

  "OtherUkDividendsAmountView in English" should {

    "Render successfully without prior data" when {

      "correctly render for an individual" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(otherDividendsAmountForm, None, taxYear, testCall)(user, messages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitle)
          welshToggleCheck("English")
          h1Check(expectedH1)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
          formPostLinkCheck(continueLink, continueButtonFormSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              None,
              taxYear,
              testCall
            )(user, messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)

          }

          "a non numeric value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              None,
              taxYear,
              testCall
            )(user, messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)

          }

          "a value bigger than £100,000,000,000 is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              None,
              taxYear,
              testCall
            )(user, messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)

          }

          "an invalid format value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "10.00.00.00")),
              None,
              taxYear,
              testCall
            )(user, messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }
        }
      }

      "correctly render for an agent" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(otherDividendsAmountForm, None,
            taxYear, testCall)(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitleAgent)
          welshToggleCheck("English")
          h1Check(expectedH1Agent)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
          formPostLinkCheck(continueLink, continueButtonFormSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "a non numeric value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)

          }

          "a value bigger than £100,000,000,000 is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)

          }

          "an invalid format value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "10.00.00.00")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }
        }
      }

    }

    "Render successfully with prior data" when {

      "correctly render for an individual" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(
            otherDividendsAmountForm.fill(priorAmount),
            Some(DividendsPriorSubmission(None, Some(priorAmount))),
            taxYear,
            testCall
          )(user, messages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitle)
          welshToggleCheck("English")
          h1Check(expectedH1)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString), youToldUsSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
          formPostLinkCheck(continueLink, continueButtonFormSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "a non numeric value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "a value bigger than £100,000,000,000 is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "an invalid format value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "100.000.00.00")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("English")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }
        }
      }

      "correctly render for an agent" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(
            otherDividendsAmountForm.fill(priorAmount),
            Some(DividendsPriorSubmission(None, Some(priorAmount))),
            taxYear,
            testCall
          )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitleAgent)
          welshToggleCheck("English")
          h1Check(expectedH1Agent)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString), youToldUsSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
          formPostLinkCheck(continueLink, continueButtonFormSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "a non numeric value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "a value bigger than £100,000,000,000 is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }

          "an invalid format value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "100.000.00.00")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), messages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("English")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString), youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
            formPostLinkCheck(continueLink, continueButtonFormSelector)
          }
        }
      }
    }
  }

  "OtherUkDividendsAmountView in Welsh" should {

    "Render successfully without prior data" when {

      "correctly render for an individual" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(otherDividendsAmountForm, None, taxYear, testCall)(user, welshMessages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitle)
          welshToggleCheck("Welsh")
          h1Check(expectedH1)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              None,
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)

          }

          "a non numeric value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              None,
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)

          }

          "a value bigger than £100,000,000,000 is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              None,
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)

          }

          "an invalid format value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "10.00.00.00")),
              None,
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueIndividual,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }
        }
      }

      "correctly render for an agent" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(otherDividendsAmountForm, None,
            taxYear, testCall)(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitleAgent)
          welshToggleCheck("Welsh")
          h1Check(expectedH1Agent)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "a non numeric value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)

          }

          "a value bigger than £100,000,000,000 is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)

          }

          "an invalid format value is passed in" which {

            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "10.00.00.00")),
              None,
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)

            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(tellUsTheValueAgent,tellUsTheValueSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }
        }
      }

    }

    "Render successfully with prior data" when {

      "correctly render for an individual" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(
            otherDividendsAmountForm.fill(priorAmount),
            Some(DividendsPriorSubmission(None, Some(priorAmount))),
            taxYear,
            testCall
          )(user, welshMessages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitle)
          welshToggleCheck("Welsh")
          h1Check(expectedH1)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString),youToldUsSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "a non numeric value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "a value bigger than £100,000,000,000 is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "an invalid format value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "100.000.00.00")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user, welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitle)
            welshToggleCheck("Welsh")
            h1Check(expectedH1)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextIndividual(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }
        }
      }

      "correctly render for an agent" when {

        "there are no form errors" which {

          lazy val view = otherDividendsAmountView(
            otherDividendsAmountForm.fill(priorAmount),
            Some(DividendsPriorSubmission(None, Some(priorAmount))),
            taxYear,
            testCall
          )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)
          implicit lazy val document: Document = Jsoup.parse(view.body)

          titleCheck(expectedTitleAgent)
          welshToggleCheck("Welsh")
          h1Check(expectedH1Agent)
          textOnPageCheck(expectedCaption, captionSelector)
          textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString),youToldUsSelector)
          hintTextCheck(expectedHintText)
          textOnPageCheck(poundPrefixText, poundPrefixSelector)
          inputFieldCheck(amountInputName, inputSelector)
          buttonCheck(continueText, continueButtonSelector)
        }

        "there are form errors" when {

          "an empty value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount of dividends received from trusts or investment companies"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "a non numeric value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "abc")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount using numbers 0 to 9"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "a value bigger than £100,000,000,000 is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "200,000,000,000")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter an amount less than £100,000,000,000"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }

          "an invalid format value is passed in" which {
            lazy val view = otherDividendsAmountView(
              otherDividendsAmountForm.bind(Map("amount" -> "100.000.00.00")),
              Some(DividendsPriorSubmission(None, Some(priorAmount))),
              taxYear,
              testCall
            )(user.copy(arn = Some("XARN1234567")), welshMessages, mockAppConfig)
            implicit lazy val document: Document = Jsoup.parse(view.body)

            val expectedErrorText = "Enter the amount in the correct format"

            titleCheck(expectedErrorTitleAgent)
            welshToggleCheck("Welsh")
            h1Check(expectedH1Agent)
            textOnPageCheck(expectedCaption, captionSelector)
            textOnPageCheck(youToldUsPriorTextAgent(priorAmount.toString),youToldUsSelector)
            hintTextCheck(expectedHintText)
            errorSummaryCheck(expectedErrorText, newAmountInput)
            errorAboveElementCheck(expectedErrorText)
            textOnPageCheck(poundPrefixText, poundPrefixSelector)
            inputFieldCheck(amountInputName, inputSelector)
            buttonCheck(continueText, continueButtonSelector)
          }
        }
      }
    }
  }
}
