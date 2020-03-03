import org.scalatest._

trait DefaultMenu {
  val defaultMenu: Menu = Menu(
      MenuItems("Cola", isDrink = true, isPremium = false, "Cold", 0.5) ::
      MenuItems("Coffee", isDrink = true, isPremium = false, "Hot", 1) ::
      MenuItems("Cheese Sandwich", isDrink = false, isPremium = false, "Cold", 2) ::
      MenuItems("Ham Sandwich", isDrink = false, isPremium = false, "Hot", 4.5) ::
      MenuItems("Lobster", isDrink = false, isPremium = true, "Hot", 25) ::
      Nil)
}

trait MenuForRoundingCase {
  val menu: Menu = Menu(
    MenuItems("Straw", isDrink = false, isPremium = false, "Cold", 0.05) :: Nil)
}

class CafeShopSpec extends FlatSpec {

  "Cafe Shop" should "have a Menu" in new DefaultMenu {
    assertCompiles("defaultMenu")
  }

  "Standard Bill" should "be able to produce a total of the passed item list" in new DefaultMenu {
    private val total = StandardBill(defaultMenu, "Cola", "Coffee", "Cheese Sandwich").total
    assert(total == 3.5)
  }
  it should "produce 0 when passed an empty item list" in new DefaultMenu {
    private val total = StandardBill(defaultMenu).total
    assert(total == 0)
  }
  it should "ignore item(s) not in the menu when calculating the total" in new DefaultMenu {
    private val total = StandardBill(defaultMenu, "No Such Item", "No Such Item 2", "Cola").total
    assert(total == 0.5)
  }
  it should "be able to calculate Service Charge" in new DefaultMenu {
    private val bill = StandardBill(defaultMenu)
    assertCompiles("bill.serviceCharge")
  }
  it should "not calculate service charge when all purchased items are drinks" in new DefaultMenu {
    private val serviceCharge = StandardBill(defaultMenu, "Cola", "Coffee").serviceCharge
    assert(serviceCharge == 0)
  }
  it should "calculate 10% service charge rounded to 2 decimal places, when purchased items include any food" in new DefaultMenu {
    private val bill = StandardBill(defaultMenu, "Cola", "Coffee", "Cheese Sandwich")
    private val expectedServiceCharge = (bill.total * 0.1).setScale(2, bill.roundingMode)
    assert(bill.serviceCharge == expectedServiceCharge)
  }
  it should "calculate service charge 0.05 for items: 11 x \"Straw\" cost 5p each" in new MenuForRoundingCase {
    private val numberOfItems = 11
    private val bill = StandardBill(menu, List.fill(numberOfItems)("Straw"):_*)
    private val expectedServiceCharge = BigDecimal(numberOfItems * 0.05 * 0.1).setScale(2, bill.roundingMode)
    assert(bill.serviceCharge == expectedServiceCharge)
  }
  it should "calculate service charge 0.05 for items: 9 x \"Straw\" cost 5p each" in new MenuForRoundingCase {
    private val numberOfItems = 9
    private val bill = StandardBill(menu, List.fill(numberOfItems)("Straw"):_*)
    private val expectedServiceCharge = BigDecimal(numberOfItems * 0.05 * 0.1).setScale(2, bill.roundingMode)
    assert(bill.serviceCharge == expectedServiceCharge)
  }
  it should "calculate 20% service charge, maximum 20Ł, when purchased items include any hot food" in new DefaultMenu {
    private val bill = StandardBill(defaultMenu, "Cola", "Coffee", "Ham Sandwich")
    private val expectedServiceCharge = (bill.total * 0.2).setScale(2, bill.roundingMode)
    assert(bill.serviceCharge == expectedServiceCharge)
  }
  it should "calculate service charge 1.2Ł (20%) for items: \"Cola\" 50p, \"Coffee\" 1Ł, \"Ham Sandwich\" 4.5Ł" in new DefaultMenu {
    private val serviceCharge = StandardBill(defaultMenu, "Cola", "Coffee", "Ham Sandwich").serviceCharge
    private val expectedServiceCharge = 1.2
    assert(serviceCharge == expectedServiceCharge)
  }
  it should "calculate service charge 20Ł (maximum) for items: 40 x \"Ham Sandwich\" cost 4.5Ł each" in new DefaultMenu {
    private val copiousAmount = 40
    private val serviceCharge = StandardBill(defaultMenu, List.fill(copiousAmount)("Ham Sandwich"):_*).serviceCharge
    private val expectedServiceCharge = 20
    assert(serviceCharge == expectedServiceCharge)
  }
  it should "apply a service charge of 25% to the total bill when the orders have premium item" in new DefaultMenu {
    private val serviceCharge = StandardBill(defaultMenu, List("Lobster"):_*).serviceCharge
    private val expectedServiceCharge = 6.25
    assert(serviceCharge == expectedServiceCharge)
  }
  it should "apply a maximum charge of 40Ł to the total bill when the orders have at least one premium item" in new DefaultMenu {
    private val copiousAmount = 7
    private val serviceCharge = StandardBill(defaultMenu, List.fill(copiousAmount)("Lobster"):_*).serviceCharge
    private val expectedServiceCharge = 40
    assert(serviceCharge == expectedServiceCharge)
  }


  "The Menu" should "have Cola" in new DefaultMenu {
    assert(defaultMenu.findByName("Cola").isDefined)
  }
  it should "have Coffee" in new DefaultMenu {
    assert(defaultMenu.findByName("Coffee").isDefined)
  }
  it should "have Cheese Sandwich" in new DefaultMenu {
    assert(defaultMenu.findByName("Cheese Sandwich").isDefined)
  }
  it should "have Ham Sandwich" in new DefaultMenu {
    assert(defaultMenu.findByName("Ham Sandwich").isDefined)
  }

  "Cola" should "cost 50p" in new DefaultMenu {
    private val cola = defaultMenu.findByName("Cola").get
    assert(cola.price == 0.5)
  }
  it should "be categorized as Cold" in new DefaultMenu {
    private val cola = defaultMenu.findByName("Cola").get
    assert(cola.category == "Cold")
  }
  it should "be considered as a Drink" in new DefaultMenu {
    private val cola = defaultMenu.findByName("Cola").get
    assert(cola.isDrink)
  }

  "Coffee" should "cost Ł1" in new DefaultMenu {
    private val coffee = defaultMenu.findByName("Coffee").get
    assert(coffee.price == 1)
  }
  it should "be categorized as Hot" in new DefaultMenu {
    private val coffee = defaultMenu.findByName("Coffee").get
    assert(coffee.category == "Hot")
  }
  it should "be considered as a Drink" in new DefaultMenu {
    private val coffee = defaultMenu.findByName("Coffee").get
    assert(coffee.isDrink)
  }

  "Cheese Sandwich" should "cost Ł2" in new DefaultMenu {
    private val cheeseSandwich = defaultMenu.findByName("Cheese Sandwich").get
    assert(cheeseSandwich.price == 2)
  }
  it should "be categorized as Cold" in new DefaultMenu {
    private val cheeseSandwich = defaultMenu.findByName("Cheese Sandwich").get
    assert(cheeseSandwich.category == "Cold")
  }
  it should "not be considered as a Drink" in new DefaultMenu {
    private val cheeseSandwich = defaultMenu.findByName("Cheese Sandwich").get
    assert(!cheeseSandwich.isDrink)
  }

  "Ham Sandwich" should "cost Ł4.50" in new DefaultMenu {
    private val hamSandwich = defaultMenu.findByName("Ham Sandwich").get
    assert(hamSandwich.price == 4.5)
  }
  it should "be categorized as Hot" in new DefaultMenu {
    private val hamSandwich = defaultMenu.findByName("Ham Sandwich").get
    assert(hamSandwich.category == "Hot")
  }
  it should "not be considered as a Drink" in new DefaultMenu {
    private val hamSandwich = defaultMenu.findByName("Ham Sandwich").get
    assert(!hamSandwich.isDrink)
  }

  "Lobster" should "cost Ł25" in new DefaultMenu {
    private val lobster = defaultMenu.findByName("Lobster").get
    assert(lobster.price == 25)
  }
  it should "be categorized as Hot" in new DefaultMenu {
    private val lobster = defaultMenu.findByName("Lobster").get
    assert(lobster.category == "Hot")
  }
  it should "not be considered as a Drink" in new DefaultMenu {
    private val lobster = defaultMenu.findByName("Lobster").get
    assert(!lobster.isDrink)
  }
  it should "be a premium item" in new DefaultMenu {
    private val lobster = defaultMenu.findByName("Lobster").get
    assert(lobster.isPremium)
  }

}
