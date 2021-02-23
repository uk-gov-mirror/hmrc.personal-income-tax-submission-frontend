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
import play.twirl.api.HtmlFormat
import utils.ViewTest
import views.html.templates.NotFoundTemplate

class NotFoundTemplateSpec extends ViewTest {

  object Selectors {

    val h1Selector = "#main-content > div > div > header > h1"
    val p1Selector = "#main-content > div > div > p:nth-child(2)"
    val p2Selector = "#main-content > div > div > p:nth-child(3)"
    val p3Selector = "#main-content > div > div > p:nth-child(4)"
  }

  val h1Expected = "Page not found"
  val p1Expected = "If you typed the web address, check it is correct."
  val p2Expected = "If you used ‘copy and paste’ to enter the web address, check you copied the full address."
  val p3Expected: String = "If the web address is correct or you selected a link or button, you can use Self Assessment: " +
    "general enquiries (opens in new tab) to speak to someone about your income tax."
  val p3ExpectedLink = "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/self-assessment"
  val expectedTitle = s"$h1Expected - $serviceName - $govUkExtension"

  val notFoundTemplate: NotFoundTemplate = app.injector.instanceOf[NotFoundTemplate]
  val appConfig: AppConfig = mockAppConfig

  lazy val view: HtmlFormat.Appendable = notFoundTemplate()(fakeRequest, messages, mockAppConfig)
  implicit lazy val document: Document = Jsoup.parse(view.body)

  "NotFoundTemplate" should {

    "render the page correctly" which {

      "has the correct page title" in {

        document.title() shouldBe expectedTitle
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

      "has the correct p3" in {

        elementText(Selectors.p3Selector) shouldBe p3Expected
      }

      "has the correct link in 3rd paragraph" in {

        document.select(s"""[id=govuk-self-assessment-link]""").attr("href") shouldBe p3ExpectedLink
      }
    }
  }

}
