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

package controllers.interest

import play.api.http.Status.{BAD_REQUEST, OK, SEE_OTHER}
import play.api.mvc.Result
import utils.ViewTest
import views.html.interest.TaxedInterestAmountView

import scala.concurrent.Future

class TaxedInterestAmountControllerSpec extends ViewTest{

  lazy val controller = new TaxedInterestAmountController(mockMessagesControllerComponents,
    authorisedAction, app.injector.instanceOf[TaxedInterestAmountView])(mockAppConfig)

  val taxYear = 2020


  ".show" should {

    "return a result" which {
      s"has an OK($OK) status" in new TestWithAuth {
        val result: Future[Result] = controller.show(taxYear)(fakeRequest)

        status(result) shouldBe OK
      }
    }
  }
  ".submit" should {
    "return a result" which {
      s"has a see-other ($SEE_OTHER) status" in new TestWithAuth {
        val result: Future[Result] = controller.submit(taxYear)(fakeRequest.withFormUrlEncodedBody("friendlyName" -> "name", "incomeTaxAmount" -> "50"))

        status(result) shouldBe SEE_OTHER
        redirectUrl(result) shouldBe mockAppConfig.signInUrl
      }

      s"Has a bad request ($BAD_REQUEST) when we don't tick a box" in new TestWithAuth{
        val result: Future[Result] = controller.submit(taxYear)(fakeRequest)

        status(result) shouldBe BAD_REQUEST
      }
    }
  }
}