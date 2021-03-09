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
import models.interest.InterestAccountModel
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import utils.ViewTest
import views.html.interest.InterestAccountsView

class InterestAccountsViewSpec extends ViewTest {

  lazy val view: InterestAccountsView = app.injector.instanceOf[InterestAccountsView]

  val taxYear = 2020

  val TAXED = "taxed"
  val UNTAXED = "untaxed"

  private val untaxedYesNoForm =
    YesNoForm.yesNoForm("Select yes if you received taxed interest from the UK").bind(Map("value" -> "true"))
  private val taxedYesNoForm =
    YesNoForm.yesNoForm("Select yes if you received untaxed interest from the UK").bind(Map("value" -> "true"))

  object Selectors {
    val accountRow: Int => String = rowNumber => s".govuk-form-group > ul > li:nth-child($rowNumber)"
    val accountRowName: Int => String = rowNumber => accountRow(rowNumber) + " > span:nth-child(1)"
    val accountRowChange: Int => String = rowNumber => accountRow(rowNumber) + " > span:nth-child(2) > a"
    val accountRowRemove: Int => String = rowNumber => accountRow(rowNumber) + " > span:nth-child(3) > a"

    val accountRowChangePriorSubmission: Int => String = rowNumber => accountRow(rowNumber) + " > span:nth-child(3) > a"
    val accountRowRemovePriorSubmission: Int => String = rowNumber => accountRow(rowNumber) + " > span:nth-child(4) > a"
  }

  object ExpectedValues {
    val untaxedH1Singular = "UK untaxed interest account"
    val untaxedH1Plural = "UK untaxed interest accounts"
    val taxedH1Singular = "UK taxed interest account"
    val taxedH1Plural = "UK taxed interest accounts"

    val caption = "Interest for 6 April 2019 to 5 April 2020"

    val change = "Change"
    val remove = "Remove"

    val addAnotherAccount = "Add another account"

    val untaxedTitleSingle = s"$untaxedH1Singular - $serviceName - $govUkExtension"
    val untaxedTitlePlural = s"$untaxedH1Plural - $serviceName - $govUkExtension"
    val taxedTitleSingle = s"$taxedH1Singular - $serviceName - $govUkExtension"
    val taxedTitlePlural = s"$taxedH1Plural - $serviceName - $govUkExtension"
  }

  "InterestAccountsView when untaxed" should {

    "render with 1 row" when {

      "there is a single account passed in that is not a prior submission" which {

        lazy val result = view(untaxedYesNoForm, taxYear, Seq(
          InterestAccountModel(None, "Bank of UK", 9001.00, Some("qwerty"))
        ), UNTAXED)
        implicit val document: Document = Jsoup.parse(result.body)

        "has the correct title" in {
          assertTitle(ExpectedValues.untaxedTitleSingle)
        }

        "has the correct caption" in {
          assertCaption(ExpectedValues.caption)
        }

        "has the correct h1" in {
          assertH1(ExpectedValues.untaxedH1Singular)
        }

        "has a single account row" in {
          elementExist(Selectors.accountRow(1)) shouldBe true
          elementExist(Selectors.accountRow(2)) shouldBe false
        }

        "the row should have the correct account name" in {
          elementText(Selectors.accountRowName(1)) shouldBe "Bank of UK"
        }

        "the row should have a change link" which {

          "has the correct text" in {
            elements(Selectors.accountRowChange(1)).get(0).child(0).text shouldBe ExpectedValues.change
          }

          "has the correct hidden text" in {
            elements(Selectors.accountRowChange(1)).get(0).child(1).text shouldBe "Bank of UK account details"
          }

          "has the correct link" in {
            element(Selectors.accountRowChange(1)).attr("href") shouldBe controllers.interest.routes.UntaxedInterestAmountController
              .show(taxYear, "qwerty").url
          }

        }

        "the row should have a remove link" which {

          "has the correct text" in {
            element(Selectors.accountRowRemove(1)).child(0).text shouldBe ExpectedValues.remove
          }

          "has the correct hidden text" in {
            element(Selectors.accountRowRemove(1)).child(1).text shouldBe "Bank of UK"
          }

          "has the correct link" in {
            element(Selectors.accountRowRemove(1)).attr("href") shouldBe
              "/income-through-software/return/personal-income/2020/interest/remove-untaxed-interest-account?accountId=qwerty"
          }
        }

      }

      "there is a single account passed in that is a prior submission" which {

        lazy val result = view(untaxedYesNoForm, taxYear, Seq(
          InterestAccountModel(Some("qwerty"), "Bank of UK", 9001.00)
        ), UNTAXED)
        implicit val document: Document = Jsoup.parse(result.body)

        "has the correct title" in {
          assertTitle(ExpectedValues.untaxedTitleSingle)
        }

        "has the correct caption" in {
          assertCaption(ExpectedValues.caption)
        }

        "has the correct h1" in {
          assertH1(ExpectedValues.untaxedH1Singular)
        }

        "has a single account row" in {
          elementExist(Selectors.accountRow(1)) shouldBe true
          elementExist(Selectors.accountRow(2)) shouldBe false
        }

        "the row should have the correct account name" in {
          elementText(Selectors.accountRowName(1)) shouldBe "Bank of UK"
        }

        "the row should have a change link" which {

          "has the correct text" in {
            elements(Selectors.accountRowChangePriorSubmission(1)).get(0).child(0).text shouldBe ExpectedValues.change
          }

          "has the correct hidden text" in {
            elements(Selectors.accountRowChangePriorSubmission(1)).get(0).child(1).text shouldBe "Bank of UK account details"
          }

          "has the correct link" in {
            element(Selectors.accountRowChangePriorSubmission(1)).attr("href") shouldBe controllers.interest.routes.ChangeAccountAmountController
              .show(taxYear, "untaxed", "qwerty").url
          }

        }

      }

    }

    "render with 2 rows" when {

      "there are two accounts passed in" which {

        lazy val result = view(untaxedYesNoForm, taxYear, Seq(
          InterestAccountModel(None, "Bank of UK", 9000.01, Some("qwerty")),
          InterestAccountModel(Some("azerty"), "Bank of EU", 1234.56) // This is a prior submission
        ), UNTAXED)
        implicit val document: Document = Jsoup.parse(result.body)

        "has the correct title" in {
          assertTitle(ExpectedValues.untaxedTitlePlural)
        }

        "has the correct caption" in {
          assertCaption(ExpectedValues.caption)
        }

        "has the correct h1" in {
          assertH1(ExpectedValues.untaxedH1Plural)
        }

        "has two account rows" in {
          elementExist(Selectors.accountRow(1)) shouldBe true
          elementExist(Selectors.accountRow(2)) shouldBe true
          elementExist(Selectors.accountRow(3)) shouldBe false
        }

        "the first row" should {
          "have the correct account name" in {
            elementText(Selectors.accountRowName(1)) shouldBe "Bank of UK"
          }

          "the row should have a change link" which {

            "has the correct text" in {
              elements(Selectors.accountRowChange(1)).get(0).child(0).text shouldBe ExpectedValues.change
            }

            "has the correct hidden text" in {
              elements(Selectors.accountRowChange(1)).get(0).child(1).text shouldBe "Bank of UK account details"
            }

            "has the correct link" in {
              element(Selectors.accountRowChange(1)).attr("href") shouldBe
                controllers.interest.routes.UntaxedInterestAmountController.show(taxYear, "qwerty").url
            }

          }

          "the row should have a remove link" which {

            "has the correct text" in {
              element(Selectors.accountRowRemove(1)).child(0).text shouldBe ExpectedValues.remove
            }

            "has the correct hidden text" in {
              element(Selectors.accountRowRemove(1)).child(1).text shouldBe "Bank of UK"
            }

            "has the correct link" in {
              element(Selectors.accountRowRemove(1)).attr("href") shouldBe
                "/income-through-software/return/personal-income/2020/interest/remove-untaxed-interest-account?accountId=qwerty"
            }
          }
        }

        "the second row that is a prior submission" should {
          "have the correct account name" in {
            elementText(Selectors.accountRowName(2)) shouldBe "Bank of EU"
          }

          "the row should have a change link" which {

            "has the correct text" in {
              elements(Selectors.accountRowChangePriorSubmission(2)).get(0).child(0).text shouldBe ExpectedValues.change
            }

            "has the correct hidden text" in {
              elements(Selectors.accountRowChangePriorSubmission(2)).get(0).child(1).text shouldBe "Bank of EU account details"
            }

            "has the correct link" in {
              element(Selectors.accountRowChangePriorSubmission(2)).attr("href") shouldBe
                controllers.interest.routes.ChangeAccountAmountController.show(taxYear, "untaxed", "azerty").url
            }

          }
        }

      }

    }

  }

  "InterestAccountsView when taxed" should {

    "render with 1 row" when {

      "there is a single account passed in that is not a prior submission" which {

        lazy val result = view(taxedYesNoForm, taxYear, Seq(
          InterestAccountModel(None, "Bank of UK", 9001.00, Some("qwerty"))
        ), TAXED)
        implicit val document: Document = Jsoup.parse(result.body)

        "has the correct title" in {
          assertTitle(ExpectedValues.taxedTitleSingle)
        }

        "has the correct caption" in {
          assertCaption(ExpectedValues.caption)
        }

        "has the correct h1" in {
          assertH1(ExpectedValues.taxedH1Singular)
        }

        "has a single account row" in {
          elementExist(Selectors.accountRow(1)) shouldBe true
          elementExist(Selectors.accountRow(2)) shouldBe false
        }

        "the row should have the correct account name" in {
          elementText(Selectors.accountRowName(1)) shouldBe "Bank of UK"
        }

        "the row should have a change link" which {

          "has the correct text" in {
            elements(Selectors.accountRowChange(1)).get(0).child(0).text shouldBe ExpectedValues.change
          }

          "has the correct hidden text" in {
            elements(Selectors.accountRowChange(1)).get(0).child(1).text shouldBe "Bank of UK account details"
          }

          "has the correct link" in {
            element(Selectors.accountRowChange(1)).attr("href") shouldBe controllers.interest.routes.TaxedInterestAmountController
              .show(taxYear, "qwerty").url
          }

        }

        "the row should have a remove link" which {

          "has the correct text" in {
            element(Selectors.accountRowRemove(1)).child(0).text shouldBe ExpectedValues.remove
          }

          "has the correct hidden text" in {
            element(Selectors.accountRowRemove(1)).child(1).text shouldBe "Bank of UK"
          }

          "has the correct link" in {
            element(Selectors.accountRowRemove(1)).attr("href") shouldBe
              "/income-through-software/return/personal-income/2020/interest/remove-taxed-interest-account?accountId=qwerty"
          }
        }

      }

      "there is a single account passed in that is a prior submission" which {

        lazy val result = view(taxedYesNoForm, taxYear, Seq(
          InterestAccountModel(Some("qwerty"), "Bank of UK", 9001.00)
        ), TAXED)
        implicit val document: Document = Jsoup.parse(result.body)

        "has the correct title" in {
          assertTitle(ExpectedValues.taxedTitleSingle)
        }

        "has the correct caption" in {
          assertCaption(ExpectedValues.caption)
        }

        "has the correct h1" in {
          assertH1(ExpectedValues.taxedH1Singular)
        }

        "has a single account row" in {
          elementExist(Selectors.accountRow(1)) shouldBe true
          elementExist(Selectors.accountRow(2)) shouldBe false
        }

        "the row should have the correct account name" in {
          elementText(Selectors.accountRowName(1)) shouldBe "Bank of UK"
        }

        "the row should have a change link" which {
          "has the correct text" in {
            elements(Selectors.accountRowChangePriorSubmission(1)).get(0).child(0).text shouldBe ExpectedValues.change
          }

          "has the correct hidden text" in {
            elements(Selectors.accountRowChangePriorSubmission(1)).get(0).child(1).text shouldBe "Bank of UK account details"
          }

          "has the correct link" in {
            element(Selectors.accountRowChangePriorSubmission(1)).attr("href") shouldBe controllers.interest.routes.ChangeAccountAmountController
              .show(taxYear, "taxed", "qwerty").url
          }

          "the row should not have a remove link" in {
            elementExist(Selectors.accountRowRemovePriorSubmission(1)) shouldBe false
          }

        }

      }

    }

    "render with 2 rows" when {

      "there are two accounts passed in" which {

        lazy val result = view(taxedYesNoForm, taxYear, Seq(
          InterestAccountModel(None, "Bank of UK", 9000.01, Some("qwerty")),
          InterestAccountModel(Some("azerty"), "Bank of EU", 1234.56)
        ), TAXED)
        implicit val document: Document = Jsoup.parse(result.body)

        "has the correct title" in {
          assertTitle(ExpectedValues.taxedTitlePlural)
        }

        "has the correct caption" in {
          assertCaption(ExpectedValues.caption)
        }

        "has the correct h1" in {
          assertH1(ExpectedValues.taxedH1Plural)
        }

        "has two account rows" in {
          elementExist(Selectors.accountRow(1)) shouldBe true
          elementExist(Selectors.accountRow(2)) shouldBe true
          elementExist(Selectors.accountRow(3)) shouldBe false
        }

        "the first row" should {
          "have the correct account name" in {
            elementText(Selectors.accountRowName(1)) shouldBe "Bank of UK"
          }

          "the row should have a change link" which {

            "has the correct text" in {
              elements(Selectors.accountRowChange(1)).get(0).child(0).text shouldBe ExpectedValues.change
            }

            "has the correct hidden text" in {
              elements(Selectors.accountRowChange(1)).get(0).child(1).text shouldBe "Bank of UK account details"
            }

            "has the correct link" in {
              element(Selectors.accountRowChange(1)).attr("href") shouldBe
                controllers.interest.routes.TaxedInterestAmountController.show(taxYear, "qwerty").url
            }

          }

          "the row should have a remove link" which {

            "has the correct text" in {
              elements(Selectors.accountRowRemove(1)).get(0).child(0).text shouldBe ExpectedValues.remove
            }

            "has the correct hidden text" in {
              elements(Selectors.accountRowChange(1)).get(0).child(1).text shouldBe "Bank of UK account details"
            }

            "has the correct link" in {
              element(Selectors.accountRowRemove(1)).attr("href") shouldBe
                "/income-through-software/return/personal-income/2020/interest/remove-taxed-interest-account?accountId=qwerty"

            }
          }
        }

        "the second row" should {
          "have the correct account name" in {
            elementText(Selectors.accountRowName(2)) shouldBe "Bank of EU"
          }

          "the row should have a change link" which {

            "has the correct text" in {
              elements(Selectors.accountRowChangePriorSubmission(2)).get(0).child(0).text shouldBe ExpectedValues.change
            }

            "has the correct hidden text" in {
              elements(Selectors.accountRowChangePriorSubmission(2)).get(0).child(1).text shouldBe "Bank of EU account details"
            }

            "has the correct link" in {
              element(Selectors.accountRowChangePriorSubmission(2)).attr("href") shouldBe
                controllers.interest.routes.ChangeAccountAmountController.show(taxYear, "taxed", "azerty").url
            }

          }
        }

      }

    }

  }

}
