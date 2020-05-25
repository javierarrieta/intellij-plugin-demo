package inspections

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

class SimplifyMapInspection extends OperationOnCollectionInspection {
  setLikeCollectionClasses(Array("zio._"))

  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(MapToAsSimplification)

  object MapToAsSimplification extends SimplificationType {
    override def hint: String = "Replace with .as"

    override def getSimplification(expr: ScExpression): Option[Simplification] = {
      def replacement(qual: ScExpression, arg: ScExpression) =
        replace(expr).withText(invocationText(qual, s"as(${arg.getText}"))

      expr match {
        case qual `.map` `_ => x`(x) => Some(replacement(qual, x))
        case _                       => None
      }
    }
  }

}
