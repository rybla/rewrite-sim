# rewrite-sim

::: todo

project introduction

:::

## Usage

Requirements:

- [just](https://just.systems)

To run the web app:

```sh
just serve
```

## Design

::: todo

design description

:::

## Notes

### Displaying rewrite rules

I want to be able to rewrite rewrite rules in a shallow embedding without having to implement pattern matching and unification myself. But then there's a problem of actually displaying a rewrite rule in a readable way.

How could I slightly modify the structure of the shallow embedding so that I still don't have to implement pattern matching and unification myself, but I can also serialize a rewrite rule as a human-readable string?

Perhaps something with [deeper shallow embedding](https://drops.dagstuhl.de/storage/00lipics/lipics-vol237-itp2022/LIPIcs.ITP.2022.28/LIPIcs.ITP.2022.28.pdf)?

