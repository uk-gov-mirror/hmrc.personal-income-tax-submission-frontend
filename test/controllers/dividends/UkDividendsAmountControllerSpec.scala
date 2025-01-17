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

package controllers.dividends

import common.SessionValues
import config.AppConfig
import controllers.predicates.AuthorisedAction
import models.{DividendsCheckYourAnswersModel, DividendsPriorSubmission}
import play.api.http.Status._
import play.api.libs.json.Json
import play.api.mvc.Result
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import utils.UnitTestWithApp
import views.html.dividends.UkDividendsAmountView

import scala.concurrent.Future

class UkDividendsAmountControllerSpec extends UnitTestWithApp {

  lazy val controller = new UkDividendsAmountController()(
    mockMessagesControllerComponents,
    authorisedAction,
    app.injector.instanceOf[UkDividendsAmountView],
    mockAppConfig
  )

  val amountInputField = "amount"
  val taxYear = 2022

  ".show" should {

    val ukDividendSubmitAmount = 50

    "return a result with an OK status" when {

      "there is a prior submission in session" in new TestWithAuth {
        lazy val result: Future[Result] = {
          controller.show(taxYear)(fakeRequest.withSession(
            SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(Some(ukDividendSubmitAmount), None)).toString()
          ))
        }

        status(result) shouldBe OK
        bodyOf(result) should include("50")
        bodyOf(result) shouldNot include("govuk-error-summary")
      }

      "there is a prior submission and check your answer data in session" in new TestWithAuth {
        val amount = 100

        lazy val result: Future[Result] = {
          controller.show(taxYear)(fakeRequest.withSession(
            SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(Some(ukDividendSubmitAmount), None)).toString(),
            SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel(Some(true), Some(amount))).toString()
          ))
        }

        status(result) shouldBe OK
        bodyOf(result) should include("50")
        bodyOf(result) should include("100")
        bodyOf(result) shouldNot include("govuk-error-summary")
      }

      "there is a prior submission and check your answer data in session and not fill form when amounts are same" in new TestWithAuth {
        val amount = 50

        lazy val result: Future[Result] = {
          controller.show(taxYear)(fakeRequest.withSession(
            SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(Some(ukDividendSubmitAmount), None)).toString(),
            SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel(Some(true), Some(amount), None, None)).toString()
          ))
        }

        status(result) shouldBe OK
        bodyOf(result) should include("50")
        bodyOf(result) shouldNot include("govuk-error-summary")
      }

      "there is a prior submission in session, but no UK Dividends value" in new TestWithAuth {
        lazy val result: Future[Result] = {
          controller.show(taxYear)(fakeRequest.withSession(
            SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(None, None)).toString()
          ))
        }

        status(result) shouldBe OK
        bodyOf(result) shouldNot include("govuk-error-summary")
      }

      "there is no prior submission in session" in new TestWithAuth {
        lazy val result: Future[Result] = {
          controller.show(taxYear)(fakeRequest)
        }

        status(result) shouldBe OK
        bodyOf(result) shouldNot include("govuk-error-summary")
      }

    }

    "Redirect to the tax year error " when {

      "an invalid tax year has been added to the url" in new TestWithAuth() {

        val mockAppConfFeatureSwitch: AppConfig = new AppConfig(mock[ServicesConfig]){
          override lazy val defaultTaxYear: Int = 2022
          override lazy val taxYearErrorFeature = true
        }

        val authorisedActionFeatureSwitch = new AuthorisedAction(mockAppConfFeatureSwitch,
          agentAuthErrorPageView)(mockAuthService, stubMessagesControllerComponents())

        lazy val featureSwitchController = new UkDividendsAmountController()(
          mockMessagesControllerComponents,
          authorisedActionFeatureSwitch,
          app.injector.instanceOf[UkDividendsAmountView],
          mockAppConfFeatureSwitch
        )


        val invalidTaxYear = 2023
        lazy val result: Future[Result] = featureSwitchController.show(invalidTaxYear)(fakeRequest)

        redirectUrl(result) shouldBe controllers.routes.TaxYearErrorController.show().url

      }
    }

  }

  ".submit" should {

    val ukDividendSubmitAmount = 50

    "return errors" when {

      "the amount input does not pass validation with no prior data" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withSession(SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel(ukDividends = Some(true))).toString())
          .withFormUrlEncodedBody("amount" -> "ASDFGHJ"))

        status(result) shouldBe BAD_REQUEST
        bodyOf(result) should include("dividends.uk-dividends-amount.error.invalidFormat.individual")
      }

      "the amount input does not pass validation with prior data" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withSession(
            SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel(ukDividends = Some(true))).toString(),
            SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(Some(ukDividendSubmitAmount))).toString()
          )
          .withFormUrlEncodedBody(
            amountInputField -> "ASDFGHJ"
          ))

        status(result) shouldBe BAD_REQUEST
        bodyOf(result) should include("dividends.uk-dividends-amount.error.invalidFormat.individual")
      }

    }

    "redirect to the overview page" when {

      "there is no cya model in session and no prior submission data" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withFormUrlEncodedBody("amount" -> "120000"))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe controllers.dividends.routes.ReceiveOtherUkDividendsController.show(taxYear).url
      }

      "there is no cya model in session and but there is prior submission data" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withFormUrlEncodedBody(
            amountInputField -> "40"
          )
          .withSession(SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(Some(ukDividendSubmitAmount), None)).toString()))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe controllers.dividends.routes.DividendsCYAController.show(taxYear).url
      }

      "there is no cya model in session and and a prior submission data model with no uk dividends" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withFormUrlEncodedBody("amount" -> "40")
          .withSession(SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(None, None)).toString()))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe controllers.dividends.routes.ReceiveOtherUkDividendsController.show(taxYear).url
      }

    }

    "redirect to the receive other dividends page" when {

      "has a cya data with no uk dividends entry and no prior submission data" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withFormUrlEncodedBody("amount" -> "40")
          .withSession(SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel()).toString()))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe controllers.dividends.routes.ReceiveOtherUkDividendsController.show(taxYear).url
      }
    }

    "redirect to the check your answers page" when {

      "has a cya data with a uk dividends entry and no prior submission data" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withFormUrlEncodedBody(
            "amount" -> "40"
          )
          .withSession(SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel(
            ukDividendsAmount = Some(ukDividendSubmitAmount)
          )).toString()))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe controllers.dividends.routes.ReceiveOtherUkDividendsController.show(taxYear).url
      }

      "has a cya data with a uk dividends entry and prior submission data with an amount type of other" in new TestWithAuth {
        lazy val result: Future[Result] = controller.submit(taxYear)(fakeRequest
          .withFormUrlEncodedBody(
            amountInputField -> "50"
          )
          .withSession(
            SessionValues.DIVIDENDS_CYA -> Json.toJson(DividendsCheckYourAnswersModel(
              ukDividendsAmount = Some(ukDividendSubmitAmount)
            )).toString(),
            SessionValues.DIVIDENDS_PRIOR_SUB -> Json.toJson(DividendsPriorSubmission(
              ukDividends = Some(ukDividendSubmitAmount)
            )).toString()
          ))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe controllers.dividends.routes.DividendsCYAController.show(taxYear).url
      }
    }

  }

}
