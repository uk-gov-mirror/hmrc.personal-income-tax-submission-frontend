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

package controllers

import config.AppConfig
import controllers.predicates.AuthorisedAction
import forms.CurrencyForm
import javax.inject.Inject
import models.CurrencyModel
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.Html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import views.html.dividends.UkDividendsViewAmount

class UkDividendsAmountController @Inject()(
                                             cc: MessagesControllerComponents,
                                             authAction: AuthorisedAction,
                                             ukDividendsView: UkDividendsViewAmount,
                                             implicit val appConfig: AppConfig
                                          ) extends FrontendController(cc) with I18nSupport {

  def view(uKDividendsForm: Form[CurrencyModel])(implicit request: Request[AnyContent]): Html =
    ukDividendsView(
      ukDividendsAmountForm = uKDividendsForm,
      postAction = controllers.routes.UkDividendsAmountController.submit(),
      backUrl = ""
    )

  def show: Action[AnyContent] = authAction { implicit user =>
    Ok(view(CurrencyForm.currencyAmountForm()))
  }

  def submit: Action[AnyContent] = authAction { implicit user =>
    CurrencyForm.currencyAmountForm.bindFromRequest().fold (
      {
        formWithErrors => BadRequest(view(formWithErrors))
      },
      {
        form => Ok("Next Page")
      }
    )
  }

}

