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

package controllers.interest

import common.SessionValues
import controllers.Assets.BAD_REQUEST
import forms.UntaxedInterestAmountForm
import helpers.PlaySessionCookieBaker
import models.interest.{InterestAccountModel, InterestCYAModel}
import play.api.http.HeaderNames
import play.api.http.Status.{OK, UNAUTHORIZED}
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import utils.IntegrationTest

class UntaxedInterestAmountControllerISpec extends IntegrationTest{

  lazy val wsClient: WSClient = app.injector.instanceOf[WSClient]

  ".show" should {

    "return an action" when {

      s"has an OK($OK) status" which {

        lazy val interestCYA = InterestCYAModel(
          Some(true), Some(Seq(InterestAccountModel(Some("UntaxedId"), "Untaxed Account", 25))),
          Some(false), None
        )
        lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map(
          SessionValues.INTEREST_CYA -> Json.prettyPrint(Json.toJson(interestCYA))
        ))

        lazy val result = {
          authoriseIndividual()
          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
            .get())
        }

        s"has an OK($OK) status" in {
          result.status shouldBe OK
        }

      }

      "there is no CYA data in session" which {
        lazy val result = {
          authoriseIndividual()
          stubGet("/income-through-software/return/2020/view", OK, "<title>Overview Page</title>")


          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .get())
        }

        s"has an OK($OK) status" in {
          result.status shouldBe OK
        }

      }

      "the authorization fails" which {
        lazy val result = {
          authoriseIndividualUnauthorized()
          stubGet("/income-through-software/return/2020/view", OK, "<title>Overview Page</title>")


          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .get())
        }

        s"has an OK($UNAUTHORIZED) status" in {
          result.status shouldBe UNAUTHORIZED
        }
      }
    }
  }

  ".submit" should {

    "return an action" when {

      "there is no CYA data in session" which {
        lazy val result = {
          authoriseIndividual()
          stubGet("/income-through-software/return/2020/view", OK, "<title>Overview Page</title>")


          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .post(Map(UntaxedInterestAmountForm.untaxedAmount -> "67.66",
              UntaxedInterestAmountForm.untaxedAccountName -> "Santander")))
        }
          s"has an OK($OK) status" in {
            result.status shouldBe OK
          }
        }

      s"there is form data" which {

        lazy val interestCYA = InterestCYAModel(
          Some(true), Some(Seq(InterestAccountModel(Some("UntaxedId"), "Untaxed Account", 25))),
          Some(false), None
        )
        lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map(
          SessionValues.INTEREST_CYA -> Json.prettyPrint(Json.toJson(interestCYA))
        ))

        lazy val result = {
          authoriseIndividual()
          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
            .post(Map(UntaxedInterestAmountForm.untaxedAmount -> "67.66",
              UntaxedInterestAmountForm.untaxedAccountName -> "Santander")))
        }

        s"has an OK($OK) status" in {
          result.status shouldBe OK
        }

      }

      s"there is no form data" which {

        lazy val interestCYA = InterestCYAModel(
          Some(true), Some(Seq(InterestAccountModel(Some("UntaxedId"), "Untaxed Account", 25))),
          Some(false), None
        )
        lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map(
          SessionValues.INTEREST_CYA -> Json.prettyPrint(Json.toJson(interestCYA))
        ))

        lazy val result = {
          authoriseIndividual()
          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
            .post(Map[String, String]()))
        }

        s"has an BAD_REQUEST($BAD_REQUEST) status" in {
          result.status shouldBe BAD_REQUEST
        }

      }

      s"return an UNAUTHORIZED($UNAUTHORIZED) status" which {

        lazy val interestCYA = InterestCYAModel(
          Some(true), Some(Seq(InterestAccountModel(Some("UntaxedId"), "Untaxed Account", 25))),
          Some(false), None
        )
        lazy val sessionCookie: String = PlaySessionCookieBaker.bakeSessionCookie(Map(
          SessionValues.INTEREST_CYA -> Json.prettyPrint(Json.toJson(interestCYA))
        ))

        lazy val result = {
          authoriseIndividualUnauthorized()
          await(wsClient.url(s"$startUrl/2020/interest/untaxed-uk-interest-details/UntaxedId")
            .withHttpHeaders(HeaderNames.COOKIE -> sessionCookie, "Csrf-Token" -> "nocheck")
            .post(Map[String, String]()))
        }

        s"has an BAD_REQUEST($UNAUTHORIZED) status" in {
          result.status shouldBe UNAUTHORIZED
        }

      }

    }
  }

}