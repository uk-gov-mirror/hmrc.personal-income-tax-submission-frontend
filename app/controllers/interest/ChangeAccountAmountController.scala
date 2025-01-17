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

import common.{InterestTaxTypes, SessionValues}
import config.{AppConfig, INTEREST}
import controllers.predicates.AuthorisedAction
import controllers.predicates.CommonPredicates.commonPredicates
import forms.ChangeAccountAmountForm
import models.User
import models.interest.{InterestAccountModel, InterestCYAModel, InterestPriorSubmission}
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.libs.json.{Json, Reads}
import play.api.mvc._
import play.twirl.api.Html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import views.html.interest.ChangeAccountAmountView

import javax.inject.Inject

class ChangeAccountAmountController @Inject()(
                                             implicit val cc: MessagesControllerComponents,
                                             authAction: AuthorisedAction,
                                             changeAccountAmountView: ChangeAccountAmountView,
                                             implicit val appConfig: AppConfig
                                           ) extends FrontendController(cc) with I18nSupport {

  def view(
            formInput: Form[BigDecimal],
            priorSubmission: InterestAccountModel,
            taxYear: Int,
            taxType: String,
            accountId: String,
            preAmount:Option[BigDecimal] = None
          )(implicit user: User[AnyContent]): Html = {

    changeAccountAmountView(
      form = formInput,
      postAction = controllers.interest.routes.ChangeAccountAmountController.submit(taxYear, taxType, accountId),
      taxYear = taxYear,
      taxType = taxType,
      account = priorSubmission,
      preAmount = preAmount)
  }

  def show(taxYear: Int, taxType: String, accountId: String): Action[AnyContent] = commonPredicates(taxYear, INTEREST).apply { implicit user =>
    val interestPriorSubmissionSession = getSessionData[InterestPriorSubmission](SessionValues.INTEREST_PRIOR_SUB)
    val checkYourAnswerSession: Option[InterestCYAModel] = getSessionData[InterestCYAModel](SessionValues.INTEREST_CYA)

    val singleAccount: Option[InterestAccountModel] = getSingleAccount(accountId, interestPriorSubmissionSession)

    (singleAccount, checkYourAnswerSession) match {
      case (None, Some(_)) => Redirect(controllers.interest.routes.AccountsController.show(taxYear, taxType))
      case (Some(accountModel), Some(cya)) =>

        val previousAmount: Option[BigDecimal] = extractPreAmount(taxType,Some(cya),accountId)

        val form: Form[BigDecimal] = {
          if(previousAmount.contains(accountModel.amount)) {
            ChangeAccountAmountForm.changeAccountAmountForm(taxType)
          }
            else{
            ChangeAccountAmountForm.changeAccountAmountForm(taxType).
              fill(previousAmount.getOrElse(accountModel.amount))
          }
        }

        Ok(view(form, accountModel, taxYear, taxType, accountId, Some(previousAmount.getOrElse(accountModel.amount))))
      case _ => Redirect(appConfig.incomeTaxSubmissionOverviewUrl(taxYear))
    }
  }

  def submit(taxYear: Int, taxType: String, accountId: String): Action[AnyContent] = authAction { implicit user =>
    val interestPriorSubmissionSession = getSessionData[InterestPriorSubmission](SessionValues.INTEREST_PRIOR_SUB)
    val checkYourAnswerSession = getSessionData[InterestCYAModel](SessionValues.INTEREST_CYA)

    val singleAccount: Option[InterestAccountModel] = getSingleAccount(accountId, interestPriorSubmissionSession)

    checkYourAnswerSession match {
      case Some(cyaData) =>
        singleAccount match {
          case Some(account) =>
            val previousAmount = extractPreAmount(taxType,Some(cyaData),accountId)

            ChangeAccountAmountForm.changeAccountAmountForm(taxType).bindFromRequest().fold(
              formWithErrors => BadRequest(view(formWithErrors, account, taxYear, taxType, accountId, previousAmount)),
              formModel => {
                val updatedAccounts = updateAccounts(taxType, cyaData, accountId, formModel)
                val updatedCYA = replaceAccounts(taxType, cyaData, updatedAccounts)
                Redirect(controllers.interest.routes.AccountsController.show(taxYear, taxType)).addingToSession(
                  SessionValues.INTEREST_CYA -> updatedCYA.asJsonString
                )
              }
            )
          case _ => Redirect(controllers.interest.routes.AccountsController.show(taxYear, taxType))
        }
      case _ => Redirect(controllers.interest.routes.AccountsController.show(taxYear, taxType))
    }
  }

  private def getSingleAccount(accountId: String, interestPriorSubmissionSession: Option[InterestPriorSubmission]): Option[InterestAccountModel] = {
    interestPriorSubmissionSession.flatMap { unwrappedPrior =>
      unwrappedPrior.submissions.flatMap { unwrappedAccounts =>
        unwrappedAccounts.find { account =>
          account.id.contains(accountId)
        }
      }
    }
  }

  private[interest] def replaceAccounts(taxType: String, cyaData: InterestCYAModel,
                                        accounts: Option[Seq[InterestAccountModel]]): InterestCYAModel = taxType match {
    case InterestTaxTypes.UNTAXED => cyaData.copy(untaxedUkAccounts = accounts, untaxedUkInterest = Some(true))
    case InterestTaxTypes.TAXED => cyaData.copy(taxedUkAccounts = accounts, taxedUkInterest = Some(true))
  }

  private[interest] def extractPreAmount(taxType: String, checkYourAnswerSession: Option[InterestCYAModel],
                                         accountId: String): Option[BigDecimal] = taxType match {
    case InterestTaxTypes.UNTAXED => checkYourAnswerSession.flatMap { unwrappedCya =>
      unwrappedCya.untaxedUkAccounts.flatMap { unwrappedAccounts =>
        unwrappedAccounts.find { account =>
          account.id.contains(accountId) || account.uniqueSessionId.contains(accountId)
        }.map(_.amount)
      }
    }
    case InterestTaxTypes.TAXED => checkYourAnswerSession.flatMap { unwrappedCya =>
      unwrappedCya.taxedUkAccounts.flatMap { unwrappedAccounts =>
        unwrappedAccounts.find { account =>
          account.id.contains(accountId) || account.uniqueSessionId.contains(accountId)
        }.map(_.amount)
      }
    }
  }

  private[interest] def updateAccounts(taxType: String, cya: InterestCYAModel, accountId: String,
                                       newAmount: BigDecimal):Option[Seq[InterestAccountModel]] = taxType match {
    case InterestTaxTypes.UNTAXED =>
      cya.untaxedUkAccounts.map { unwrappedAccounts =>
        unwrappedAccounts.map { account =>
          if (account.id.contains(accountId) || account.uniqueSessionId.contains(accountId)) {
            account.copy(amount = newAmount)
          } else {
            account
          }
        }
      }

    case InterestTaxTypes.TAXED =>
      cya.taxedUkAccounts.map { unwrappedAccounts =>
        unwrappedAccounts.map { account =>
          if(account.id.contains(accountId) || account.uniqueSessionId.contains(accountId)){
            account.copy(amount = newAmount)
          } else {
            account
          }
        }
      }

  }

  private[interest] def getSessionData[T](key: String)(implicit user: User[_], reads: Reads[T]): Option[T] = {
    user.session.get(key).flatMap { stringValue =>
      Json.parse(stringValue).asOpt[T]
    }
  }

}
