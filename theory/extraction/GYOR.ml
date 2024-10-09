
type green_hue =
| SomeGreen
| NoGreen

type yellow_hue =
| SomeYellow
| NoYellow

type orange_hue =
| SomeOrange
| NoOrange

type red_hue =
| SomeRed
| NoRed

type color =
| Mix of green_hue * yellow_hue * orange_hue * red_hue
