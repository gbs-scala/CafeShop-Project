import scala.math.BigDecimal
import BigDecimal.RoundingMode.Value
import scala.math.BigDecimal.RoundingMode.HALF_UP

case class CafeShop(menu: Menu)

case class Menu(items: List[MenuItems]) {
  private val dictionary: Map[String, MenuItems] =
    items.foldLeft(Map[String, MenuItems]())((d, menuItem) => d + (menuItem.name -> menuItem))

  def findByName(itemName: String): Option[MenuItems] = dictionary.get(itemName)
  def priceValue(itemName: String):Option[BigDecimal] = dictionary.get(itemName).map(_.price)
}

case class MenuItems(name: String, drinks: Boolean, premium: Boolean, category: String, price: BigDecimal)

case class StandardBill(menu: Menu, itemNames: String*) {

  val roundingMode: Value = HALF_UP
  val total: BigDecimal = itemNames.flatMap(menu.priceValue).sum
  val serviceCharge: BigDecimal = calculateServiceCharge(analyzeOrders(itemNames.toList))

  case class BillAnalysisResult(drinksOnly: Boolean, hotFood: Boolean, premiumItem: Boolean)

  protected def analyzeOrders(orders: List[String]): BillAnalysisResult = {
    itemNames
      .flatMap(menu.findByName)
      .foldLeft(BillAnalysisResult(drinksOnly = true, hotFood = false, premiumItem = false)) {
        (result, menuItem) => BillAnalysisResult(
          result.drinksOnly & menuItem.drinks,
          result.hotFood | (menuItem.category == "Hot" && !menuItem.drinks),
          result.premiumItem | menuItem.premium
        )
      }
  }

  protected def calculateServiceCharge(result: BillAnalysisResult): BigDecimal = {
    if(result.premiumItem) { (total * 0.25).setScale(2, roundingMode) min 40 }
    else if(result.drinksOnly) { 0 }
    else if(!result.hotFood) { (total * 0.1).setScale(2, roundingMode) }
    else { (total * 0.2).setScale(2, roundingMode) min 20 }
  }
}
