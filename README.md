# Coya Challenge

## Our Problem

At Coya we're working hard to make a quote processor.

A quote processor will be an object that will get information about a potential customer and the product that they want to insure, and will return the insurance premium (or whether we even want to offer the customer insurance).

Examples:

- if Arya Stark ask for insurance of her small playing sword, we may decide to insure it for 50 € a year, as the sword has some value and will be used in fencing lessons (but otherwise should be safe)
- if Luke Skywalker asks for insurance of his right hand (that he values a lot), we may decide to insure it for 2 € a year, as his hand is not that valuable and he lives a very calm life in Tatooine
- if James Bond asks for insurance of his car, we may decide not to insure it at all, as our investigation shows a very high probability of the car somehow breaking

## Current data model

To work towards this, we already have a set of models:

- A user (with an id, an address, and a risk value)
- An address (with an id and a location risk value)
- A product (with an id and a price value)

(Risk values are in our patented Universal Risk Value Scale [tm])

For products we have modelled in our system the following types:

- Houses (with address and size in square meters)
- Bananas (with number of black spots)
- Bicycles (with number of gears)

## Rules

We want to build a quote processor that will receive a user and a set of products, and return whether we reject the user or not, and if not, for how much premium will we insure the products in euros per year.

To ensure this, we have a set of rules to follow.

Each product will have a base premium value, that multiplied by the product value will give a subtotal for the product's premium. To this subtotal, we must add a surcharge that depends on the product type and maybe on other rules. Each surcharge is multiplied to the base value.

Then, for all the products, if for one the rules say that it is un-insurable, then the final result given is that we won't offer an insurance. If no product is un-insurable, then we add all the premiums and that is the quote we return to the user.

### Base surcharge value per product

| Product    | Base premium multiplier |
|------------|-------------------------|
| House      | 0.03                    |
| Banana     | 1.15                    |
| Bicycle    | 0.10                    |
| Helicopter | 0.05                    |

### Surcharges

#### User

Depending on our estimation of the risk value of the user, we apply the following surcharges:

| Universal Risk Value | Surcharge                   |
|----------------------|-----------------------------|
| Less than 20         | 0.3                         |
| From 21 to 200       | 1                           |
| From 201 to 500      | 3                           |
| 501 or more          | We won't offer an insurance |

#### House

- If the price value of _any_ house is bigger than 10_000_000, _all_ houses have a surcharge of 1.15
- If size of a house is of more than 1000 square meters, or less than 30, we won't insure it.
- There's another surcharge depending on the risk value of the address of the house:

| Universal Risk Value | Surcharge                   |
|----------------------|-----------------------------|
| Less than 100        | 0.7                         |
| From 100 to 299      | 1                           |
| From 300 to 501      | 2.5                         |
| 502 or more          | We won't offer an insurance |

#### Banana

- We won't offer insurance for bananas unless their number of black spots is between 3 and 12 (inclusive).
- We also won't offer banana insurance to users with a risk value of more than 200 (our investigations show they tend to lose them)

#### Bicycle

- There's a surcharge of `number of gears * 0.08`
- We won't offer bicycle insurance to users with a risk value of more than 150, if the total premium is bigger than 100 €.

## The challenge

Please, implement for us as much as possible of the quote processor in about 2 or 3 hours. We don't worry here that much about completeness as much as about code structure.

You can use any programming language you want to solve the challenge and you won't be penalized for any choice but we do provide a basic skeleton in Scala with some very basic tests (that can be run with `sbt test` if you have [sbt](http://www.scala-sbt.org/) installed).

We recommend you to extend these tests a bit, or if you don't use Scala to provide tests of your own. Even if you don't use Scala, they can help as examples, though.

Please commit regularly changes in git (we won't penalize experimentation, quite the opposite actually) and when you finish, send us a git bundle. To produce it, run, from inside the repository, the following:

```
$ git bundle create $myName-coya-challenge.git master
```

This will generate a git bundle file that you can send us.

Happy hacking!
