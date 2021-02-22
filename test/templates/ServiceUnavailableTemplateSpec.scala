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

package templates

import config.AppConfig
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import utils.ViewTest
import views.html.templates.ServiceUnavailableTemplate

class ServiceUnavailableTemplateSpec extends ViewTest {

  object Selectors {

    val h1Selector = "#main-content > div > div > header > h1"
    val p1Selector = "#main-content > div > div > p:nth-child(2)"
    val p2Selector = "#main-content > div > div > p:nth-child(3)"
    val bulletPoint1 = "#main-content > div > div > ul > li:nth-child(1) > p"
    val bulletPoint1Link = "#main-content > div > div > ul > li:nth-child(1) > p > a"
    val bulletPoint2 = "#main-content > div > div > ul > li:nth-child(2) > p"

  }

  val pageTitle = "Sorry, the service is unavailable - GOV.UK"
  val h1Expected = "Sorry, the service is unavailable"
  val p1Expected = "You will be able to use this service later."
  val p2Expected = "You can also:"
  val bulletPoint1Expected = "go to the Income Tax home page (opens in new tab) for more information"
  val bulletPoint1Link = "https://www.gov.uk/income-tax"
  val bulletPoint2Expected = "use Self Assessment: general enquiries (opens in new tab) to speak to someone about your income tax"
  val bulletPoint2Link = "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/self-assessment"

  lazy val serviceUnavailableTemplate: ServiceUnavailableTemplate = app.injector.instanceOf[ServiceUnavailableTemplate]
  lazy val appConfig: AppConfig = mockAppConfig


  lazy val view = serviceUnavailableTemplate()(fakeRequest, messages, appConfig)
  implicit lazy val document: Document = Jsoup.parse(view.body)

  "ServiceUnavailableTemplate" should {

    "render the page correct" which {

      "has the correct title" in {

        document.title() shouldBe pageTitle
      }

      "has the correct heading" in {

        elementText(Selectors.h1Selector) shouldBe h1Expected
       }

      "has the correct p1" in {

        elementText(Selectors.p1Selector) shouldBe p1Expected
      }

      "has the correct p2" in {

        elementText(Selectors.p2Selector) shouldBe p2Expected

      }

      "has the correct 1st bullet point" in {

        elementText(Selectors.bulletPoint1) shouldBe bulletPoint1Expected
      }

      "has the correct link in the 1st bullet point" in {

        document.select(s"""[id=govuk-income-tax-link]""").attr("href") shouldBe bulletPoint1Link
      }

      "has the correct 2nd bullet point" in {

        elementText(Selectors.bulletPoint2) shouldBe bulletPoint2Expected
      }

      "has the correct link in the 2nd bullet point" in {

        document.select(s"""[id=govuk-self-assessment-link]""").attr("href") shouldBe bulletPoint2Link
      }

    }
  }
}