inv <-  defData(varname="Inventory",dist="nonrandom",formula = 7,id="idnum")

inv <-defData(inv,varname="invoice" ,dist="uniform",formula = "1;1000")

inv <- defData(inv,varname="sku",dist = "categorical",formula = "10000;11000")

inv <- defData(inv,varname = "quantity",dist = "poisson",formula = "Inventory -1")

InventoryFile <- genData(1000,inv)