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

package views.interest

import models.interest.{InterestAccountModel, InterestCYAModel}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import utils.ViewTest
import views.html.interest.InterestCYAView

class InterestCYAViewSpec extends ViewTest {

  lazy val view: InterestCYAView = app.injector.instanceOf[InterestCYAView]

  val taxYear = 2020

  val question2 = 2
  val question4 = 4

  val account1 = 1
  val account2 = 2

  object Selectors {
    val titleSelector = "title"
    val h1Selector = "h1"
    val captionSelector = ".govuk-caption-l"
    val submitButton = ".govuk-button"

    val questionSelector: Int => String = questionNumber => s".govuk-summary-list__row:nth-child($questionNumber) > .govuk-summary-list__key"

    val questionAccountSelector: (Int, Int) => String = (questionNumber, accountNumber) => s"#question${questionNumber}account:nth-child($accountNumber)"

    val yesNoQuestionAnswer: Int => String = questionNumber => s"#main-content > div > div > dl > div:nth-child($questionNumber) > dd.govuk-summary-list__value"
  }

  object ExpectedResult {
    val titleExpected = "Check your answers - Register your income tax return with HMRC - Gov.UK"
    val h1Expected = "Check your answers"
    val captionExpected = "Interest for 06 April 2019 to 05 April 2020"

    val changeLinkExpected = "Change"
    val yesNoExpectedAnswer: Boolean => String = isYes => if(isYes) "Yes" else "No"

    val questionUntaxedInterestExpected = "Untaxed UK Interest?"
    val questionUntaxedInterestDetailsExpected = "Details for the untaxed UK interest?"
    val questionTaxedInterestExpected = "Taxed UK Interest?"
    val question4TaxedInterestDetailExpected = "Details for the taxed UK interest?"

    val untaxedInterestAccount1ExpectedTest = "UntaxedBank1 : £100.00"
    val taxedInterestAccount1ExpectedTest = "TaxedBank1 : £200.00"
    val taxedInterestAccount2ExpectedTest = "TaxedBank2 : £400.00"

    val Yes = "Yes"
    val No = "No"
  }

  "InterestCYAView" should {

    "render with all fields" when {

      "all fields are present" which {
        val cyaModel = InterestCYAModel(
          untaxedUkInterest = Some(true),
          untaxedUkAccounts = Some(Seq(InterestAccountModel("id", "UntaxedBank1", 100.00))),
          taxedUkInterest = Some(true),
          taxedUkAccounts = Some(Seq(
            InterestAccountModel("id", "TaxedBank1", 200.00),
            InterestAccountModel("id", "TaxedBank2", 400.00)
          ))
        )

        val render = view(cyaModel, taxYear)(fakeRequest, messages, mockAppConfig).body
        implicit val document: Document = Jsoup.parse(render)

        "has the correct title" in {
          assertTitle(ExpectedResult.titleExpected)
        }

        "has the correct h1" in {
          assertH1(ExpectedResult.h1Expected)
        }

        "has the correct caption" in {
          elementText(Selectors.captionSelector) shouldBe ExpectedResult.captionExpected
        }

        "the submit button" which {

          "exist" in {
            elementExist(Selectors.submitButton) shouldBe true
          }

          //TODO update when wired up
          "has the correct link" in pending

        }

        "has the correct question 1 text" in {
          elementText(Selectors.questionSelector(1)) shouldBe ExpectedResult.questionUntaxedInterestExpected
        }

        "has the correct question 2 text" in {
          elementText(Selectors.questionSelector(2)) shouldBe ExpectedResult.questionUntaxedInterestDetailsExpected
        }

        "has the correct question 3 text" in {
          elementText(Selectors.questionSelector(3)) shouldBe ExpectedResult.questionTaxedInterestExpected
        }

        "has the correct question 4 text" in {
          //noinspection ScalaStyle
          elementText(Selectors.questionSelector(4)) shouldBe ExpectedResult.question4TaxedInterestDetailExpected
        }

        "question 1 answer should be Yes" in {
          elementText(Selectors.yesNoQuestionAnswer(1)) shouldBe ExpectedResult.Yes
        }

        "question 3 answer should be Yes" in {
          elementText(Selectors.yesNoQuestionAnswer(3)) shouldBe ExpectedResult.Yes
        }

        "has the correct question 2 account text" in {
          elementText(Selectors.questionAccountSelector(question2, account1)) shouldBe ExpectedResult.untaxedInterestAccount1ExpectedTest
        }

        "has the correct question 4 account 1 text" in {
          elementText(Selectors.questionAccountSelector(question4, account1)) shouldBe ExpectedResult.taxedInterestAccount1ExpectedTest
        }

        "has the correct question 4 account 2 text" in {
          elementText(Selectors.questionAccountSelector(question4, account2)) shouldBe ExpectedResult.taxedInterestAccount2ExpectedTest
        }

      }

    }

    "renders only the yes/no questions" when {

      "the user has selected no to receiving taxed and untaxed interest" which {
        val cyaModel = InterestCYAModel(
          untaxedUkInterest = Some(false),
          untaxedUkAccounts = None,
          taxedUkInterest = Some(false),
          taxedUkAccounts = None
        )

        val render = view(cyaModel, taxYear)(fakeRequest, messages, mockAppConfig).body
        implicit val document: Document = Jsoup.parse(render)

        "has the correct title" in {
          assertTitle(ExpectedResult.titleExpected)
        }

        "has the correct h1" in {
          assertH1(ExpectedResult.h1Expected)
        }

        "has the correct caption" in {
          elementText(Selectors.captionSelector) shouldBe ExpectedResult.captionExpected
        }

        "the submit button" which {

          "exist" in {
            elementExist(Selectors.submitButton) shouldBe true
          }

          //TODO update when wired up
          "has the correct link" in pending

        }

        "question 1 should be the untaxed interest question" in {
          elementText(questionTextSelector(1)) shouldBe ExpectedResult.questionUntaxedInterestExpected
        }

        "question 2 should be the taxed interest question" in {
          elementText(questionTextSelector(2)) shouldBe ExpectedResult.questionTaxedInterestExpected
        }

        "question 1 answer should be No" in {
          elementText(Selectors.yesNoQuestionAnswer(1)) shouldBe ExpectedResult.No
        }

        "question 2 answer should be No" in {
          elementText(Selectors.yesNoQuestionAnswer(2)) shouldBe ExpectedResult.No
        }

        "there is no question 3" in {
          elementExist(Selectors.questionSelector(3)) shouldBe false
        }

        "there is no question 4" in {
          //noinspection ScalaStyle
          elementExist(Selectors.questionSelector(4)) shouldBe false
        }

      }

    }

  }

}
