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

import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"             %% "bootstrap-frontend-play-27" % "3.4.0",
    "uk.gov.hmrc"             %% "play-frontend-hmrc"         % "0.49.0-play-27",
    "uk.gov.hmrc"             %% "govuk-template"             % "5.65.0-play-27"
  )

  val test = Seq(
    "uk.gov.hmrc"             %% "bootstrap-test-play-27"   % "3.4.0"                % Test,
    "org.scalatest"           %% "scalatest"                % "3.2.2"                 % Test,
    "org.jsoup"               %  "jsoup"                    % "1.10.2"                % Test,
    "com.typesafe.play"       %% "play-test"                % current                 % Test,
    "com.vladsch.flexmark"    %  "flexmark-all"             % "0.35.10"               % "test, it",
    "org.scalatestplus.play"  %% "scalatestplus-play"       % "4.0.3"                 % "test, it",
    "com.github.tomakehurst"  %  "wiremock-jre8"            % "2.27.2"                % "test, it",
    "org.scalamock"           %% "scalamock"                % "4.4.0"                 % Test
  )
}