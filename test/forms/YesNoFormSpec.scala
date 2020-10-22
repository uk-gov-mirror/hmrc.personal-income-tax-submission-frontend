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

package forms

import models.formatHelpers.YesNoModel
import play.api.data.{Form, FormError}
import utils.UnitTest
import YesNoForm._
import YesNoForm.{no => nope}

class YesNoFormSpec extends UnitTest {

  val yesNoForm: Form[YesNoModel] = YesNoForm.yesNoForm("someError")

  "YesNoForm" should {

    "return a YesNoModel" when {

      "the answer is yes" in {

        val expectedResult = YesNoModel(yes)
        val result = yesNoForm.bind(Map(yesNo -> yes)).get

        result shouldBe expectedResult
      }

      "the answer is no" in {

        val expectedResult = YesNoModel(nope)
        val result = yesNoForm.bind(Map(yesNo -> nope)).get

        result shouldBe expectedResult
      }

    }

    "return a map on unbind" in {
      YesNoForm.formatter("SomeError").unbind("yes_no", YesNoModel("SomeError")) shouldBe Map("yes_no" -> "SomeError")
    }

    "return an error" when {

      "no option is returned" in {
        val expectedResult = Seq(FormError(yesNo, Seq("someError")))
        val result = yesNoForm.bind(Map[String, String]()).errors

        result shouldBe expectedResult
      }

      "an option that isn't yes or no is returned" in {
        val expectedResult = Seq(FormError(yesNo, Seq("someError")))
        val result = yesNoForm.bind(Map[String, String](yesNo -> "asdf")).errors

        result shouldBe expectedResult
      }
    }

  }
}