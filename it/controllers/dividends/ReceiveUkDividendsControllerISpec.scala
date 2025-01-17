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
import forms.YesNoForm
import helpers.PlaySessionCookieBaker
import models.DividendsCheckYourAnswersModel
import play.api.http.HeaderNames
import play.api.http.Status._
import play.api.libs.ws.{WSClient, WSResponse}
import utils.IntegrationTest

class ReceiveUkDividendsControllerISpec extends IntegrationTest {

  lazy val wsClient: WSClient = app.injector.instanceOf[WSClient]
  lazy val controller: ReceiveUkDividendsController = app.injector.instanceOf[ReceiveUkDividendsController]

  val taxYear: Int = 2022
  val amount: BigDecimal = 500
  val receiveUkDividendsUrl = s"$startUrl/$taxYear/dividends/dividends-from-uk-companies"

  "as an individual" when {

    ".show" should {

      "returns an action when data is not in session" which {
        lazy val result: WSResponse = {
          authoriseIndividual()
          await(wsClient.url(receiveUkDividendsUrl).get())
        }

        "has an OK(200) status" in {
          result.status shouldBe OK
        }

      }
      "returns an action when data is in session" which {

        val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
          SessionValues.DIVIDENDS_CYA -> DividendsCheckYourAnswersModel(ukDividends = Some(true), ukDividendsAmount = Some(amount),
            otherUkDividends = Some(true), otherUkDividendsAmount = Some(amount)).asJsonString
        ))

        lazy val result: WSResponse = {
          authoriseIndividual()
          await(wsClient.url(receiveUkDividendsUrl)
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck").get())
        }

        "has an OK(200) status" in {
          result.status shouldBe OK
        }

      }

      "returns an action when auth call fails" which {
        val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
          SessionValues.DIVIDENDS_CYA -> DividendsCheckYourAnswersModel(ukDividends = Some(true), ukDividendsAmount = Some(amount),
            otherUkDividends = Some(true), otherUkDividendsAmount = Some(amount)).asJsonString
        ))
        lazy val result: WSResponse = {
          authoriseIndividualUnauthorized()
          await(wsClient.url(receiveUkDividendsUrl)
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck").get())
        }
        "has an UNAUTHORIZED(401) status" in {
          result.status shouldBe UNAUTHORIZED
        }
      }

    }

    ".submit" should {

      s"return an OK($OK) status" when {

        "there is no CYA data in session" in {
          lazy val result: WSResponse = {
            authoriseIndividual()
            await(
              wsClient.url(receiveUkDividendsUrl)
                .post(Map(YesNoForm.yesNo -> YesNoForm.yes))
            )
          }

          result.status shouldBe OK
        }

      }

      s"return a BAD_REQUEST($BAD_REQUEST) status" in {
        lazy val result: WSResponse = {
          authoriseIndividual()
          await(wsClient.url(receiveUkDividendsUrl)
            .post(Map[String, String]()))
        }

        result.status shouldBe BAD_REQUEST
      }

      "returns an action when auth call fails" which {
        lazy val result: WSResponse = {
          authoriseIndividualUnauthorized()
          await(wsClient.url(receiveUkDividendsUrl)
            .post(Map[String, String]()))
        }
        "has an UNAUTHORIZED(401) status" in {
          result.status shouldBe UNAUTHORIZED
        }
      }

    }

  }

  "as an agent" when {

    ".show" should {

      "returns an action" which {
        lazy val result: WSResponse = {
          lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
            SessionValues.CLIENT_MTDITID -> "1234567890",
            SessionValues.CLIENT_NINO -> "AA123456A"
          ))

          authoriseAgent()
          await(wsClient.url(receiveUkDividendsUrl)
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie)
            .get())
        }

        "has an OK(200) status" in {
          result.status shouldBe OK
        }
      }

      "returns an action when auth call fails" which {
        val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
          SessionValues.DIVIDENDS_CYA -> DividendsCheckYourAnswersModel(ukDividends = Some(true), ukDividendsAmount = Some(amount),
            otherUkDividends = Some(true), otherUkDividendsAmount = Some(amount)).asJsonString
        ))
        lazy val result: WSResponse = {
          authoriseAgentUnauthorized()
          await(wsClient.url(receiveUkDividendsUrl)
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie).get())
        }
        "has an UNAUTHORIZED(401) status" in {
          result.status shouldBe UNAUTHORIZED
        }
      }
    }

    ".submit" should {

      s"return an OK($OK) status" when {

        "there is form data" in {
          lazy val result: WSResponse = {
            lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
              SessionValues.CLIENT_MTDITID -> "1234567890",
              SessionValues.CLIENT_NINO -> "AA123456A"
            ))

            authoriseAgent()
            await(
              wsClient.url(receiveUkDividendsUrl)
                .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
                .post(Map(YesNoForm.yesNo -> YesNoForm.yes))
            )
          }

          result.status shouldBe OK
        }
      }

      s"return a BAD_REQUEST($BAD_REQUEST) status" when {

        "there is no form data" in {
          lazy val result: WSResponse = {
            lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
              SessionValues.CLIENT_MTDITID -> "1234567890",
              SessionValues.CLIENT_NINO -> "AA123456A"
            ))

            authoriseAgent()
            await(wsClient.url(receiveUkDividendsUrl)
              .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
              .post(Map[String, String]()))
          }

          result.status shouldBe BAD_REQUEST
        }
      }

      "returns an action when auth call fails" when {
        lazy val result: WSResponse = {
          lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map[String, String](
            SessionValues.CLIENT_MTDITID -> "1234567890",
            SessionValues.CLIENT_NINO -> "AA123456A"
          ))

          authoriseAgentUnauthorized()
          await(wsClient.url(receiveUkDividendsUrl)
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
            .post(Map[String, String]()))
        }

        "has an UNAUTHORIZED(401) status" in {
          result.status shouldBe UNAUTHORIZED
        }
      }

    }

  }

}
