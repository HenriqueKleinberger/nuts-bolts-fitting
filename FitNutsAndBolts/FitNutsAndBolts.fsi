




module Assignment

type ToolType =
    | Nut
    | Bolt

type Item =
    {
      Size: int
      ToolType: ToolType
    }

val FitBoltsAndNuts: items: Item list -> (Item * Item) list

