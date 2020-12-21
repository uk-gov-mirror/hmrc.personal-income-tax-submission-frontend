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

import common.{InterestTaxTypes, PageLocations, SessionValues}
import config.AppConfig
import controllers.predicates.AuthorisedAction
import javax.inject.Inject
import models.User
import models.interest.{InterestCYAModel, InterestPriorSubmission}
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import utils.InterestSessionHelper
import views.html.interest.InterestCYAView

import scala.concurrent.Future

class InterestCYAController @Inject()(
                                       mcc: MessagesControllerComponents,
                                       authorisedAction: AuthorisedAction,
                                       interestCyaView: InterestCYAView
                                     )
                                     (
                                       implicit appConfig: AppConfig
                                     ) extends FrontendController(mcc) with I18nSupport with InterestSessionHelper {

  private val logger = Logger.logger

  def backLink(taxYear: Int)(implicit request: Request[_]): Option[String] = {
    getFromSession(SessionValues.PAGE_BACK_CYA) match {
      case location@Some(_) => location
      case _ => Some(appConfig.incomeTaxSubmissionOverviewUrl(taxYear))
    }
  }

  def show(taxYear: Int): Action[AnyContent] = authorisedAction.async { implicit user =>
    val priorSubmission = getModelFromSession[InterestPriorSubmission](SessionValues.INTEREST_PRIOR_SUB)
    val cyaModel = getCyaModel()

    cyaModel match {
      case Some(cyaData) =>
        val pageLocation = PageLocations.Interest.cya(taxYear)

        Future.successful(
          Ok(interestCyaView(cyaData, taxYear, priorSubmission, backLink(taxYear)))
            .addingToSession(
              SessionValues.INTEREST_CYA -> cyaData.asJsonString
            )
            .updateAccountsOverviewRedirect(pageLocation, InterestTaxTypes.UNTAXED)
            .updateAccountsOverviewRedirect(pageLocation, InterestTaxTypes.TAXED)
        )
      case _ =>
        logger.info("[InterestCYAController][show] No CYA data in session. Redirecting to the overview page.")
        Future.successful(Redirect(appConfig.incomeTaxSubmissionOverviewUrl(taxYear)))
    }
  }

  def submit(taxYear: Int): Action[AnyContent] = authorisedAction.async { implicit user =>
    //TODO Submission logic lives here
    Future.successful(
      Redirect(appConfig.incomeTaxSubmissionOverviewUrl(taxYear)).clearRedirects()
    )
  }

  private[interest] def getCyaModel()(implicit user: User[_]): Option[InterestCYAModel] = {
    (getModelFromSession[InterestCYAModel](SessionValues.INTEREST_CYA), getModelFromSession[InterestPriorSubmission](SessionValues.INTEREST_PRIOR_SUB)) match {
      case (None, Some(priorData)) =>
        Some(InterestCYAModel(
          Some(priorData.hasUntaxed),
          priorData.submissions.map(_.filter(_.priorType.contains(InterestTaxTypes.UNTAXED))),
          Some(priorData.hasTaxed),
          priorData.submissions.map(_.filter(_.priorType.contains(InterestTaxTypes.TAXED)))
        ))
      case (Some(cyaData), _) => Some(cyaData)
      case _ => None
    }
  }
}
