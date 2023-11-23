add_expense <- function(expenses, item_name, item_price, purchase_date) {
  new_expense <- data.frame(ItemName = item_name, ItemPrice = item_price, PurchaseDate = purchase_date)
  expenses <- rbind(expenses, new_expense)
  return(expenses)
}

calculate_balance <- function(expenses, initial_balance) {
  total_expenses <- sum(expenses$ItemPrice)
  balance <- initial_balance - total_expenses
  return(balance)
}

view_expenses <- function(expenses) {
  cat("\nExpenses:\n")
  cat("Item Name   |  Item Price  |  Purchase Date\n")
  cat("------------------------------------------\n")
  for (i in 1:nrow(expenses)) {
    cat(sprintf("%-10s | %12.2f | %s\n", expenses[i, "ItemName"], expenses[i, "ItemPrice"], expenses[i, "PurchaseDate"]))
  }
}

main <- function() {
  expenses <- data.frame(ItemName = character(),
                         ItemPrice = numeric(),
                         PurchaseDate = character(),
                         stringsAsFactors = FALSE)
  initial_balance <- as.numeric(readline("Enter your initial balance: "))
  
  while (TRUE) {
    cat("\n1. Add Expense\n")
    cat("2. View Expenses\n")
    cat("3. View Current Balance\n")
    cat("4. Exit\n")
    
    choice <- as.numeric(readline("Enter your choice (1/2/3/4): "))
    
    if (choice == 1) {
      item_name <- readline("Enter the item name: ")
      item_price <- as.numeric(readline("Enter the item price: "))
      purchase_date <- readline("Enter the purchase date: ")
      expenses <- add_expense(expenses, item_name, item_price, purchase_date)
      cat("Expense added successfully!\n")
    } else if (choice == 2) {
      if (nrow(expenses) > 0) {
        view_expenses(expenses)
      } else {
        cat("No expenses added yet.\n")
      }
    } else if (choice == 3) {
      current_balance <- calculate_balance(expenses, initial_balance)
      cat(sprintf("Your current balance is: %.2f\n", current_balance))
    } else if (choice == 4) {
      cat("Exiting the program. Goodbye!\n")
      break
    } else {
      cat("Invalid choice. Please try again.\n")
    }
  }
}

main()
