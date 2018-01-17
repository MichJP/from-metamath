$c -> ( ) not wff |- $.
$v A B C $.
wff_A $f wff A $.
wff_B $f wff B $.
wff_C $f wff C $.
wff_not $a wff ( not A ) $.
wff_imp $a wff ( A -> B ) $.
axiom_1 $a |- ( A -> ( B -> A ) ) $.
axiom_2 $a |- ( ( A -> ( B -> C ) ) -> ( ( A -> B ) -> ( A -> C ) ) ) $.
axiom_3 $a |- ( ( ( not B ) -> ( not A ) ) -> ( ( ( not B ) -> A ) -> B ) ) $.

${
min $e |- A $.
maj $e |- ( A -> B ) $.
modus_ponens $a |- B $.
$}

step_1 $p |- ( A -> ( ( A -> A ) -> A ) ) $=
  wff_A
  wff_A
  wff_A
  wff_imp
  axiom_1
$.

step_2 $p |- ( ( A -> ( ( A -> A ) -> A ) )
            -> ( ( A -> ( A -> A ) )
            -> ( A -> A ) ) ) $=
  wff_A
  wff_A
  wff_A
  wff_imp
  wff_A
  axiom_2
$.

wff_1 $p wff ( A -> ( ( A -> A ) -> A ) ) $=
  wff_A   $( 1. wff A $)
  wff_A   $( 2. wff A $)
  wff_A   $( 3. wff A $)
  wff_imp $( 4. wff ( A -> A )
                by 2, 3
          $)
  wff_A   $( 5. wff A $)
  wff_imp $( 6. wff ( ( A -> A ) -> A )
                by 4, 5
          $)
  wff_imp $( 7. wff ( A -> ( ( A -> A ) -> A ) )
                by 1, 6
          $)
$.

wff_2 $p wff ( ( A -> ( A -> A ) )
            -> ( A -> A ) ) $=
  wff_A   $( 8. wff A $)
  wff_A   $( 9. wff A $)
  wff_A   $( 10. wff A $)
  wff_imp $( 11. wff ( A -> A )
                 by 9, 10
          $)
  wff_imp $( 12. wff ( A -> ( A -> A ) )
                 by 8, 11
          $)
  wff_A   $( 13. wff A $)
  wff_A   $( 14. wff A $)
  wff_imp $( 15. wff ( A -> A )
                 by 13, 14
          $)
  wff_imp $( 16. wff ( ( A -> ( A -> A ) ) -> ( A -> A ) )
                 by 12, 15
          $)
$.

step_3 $p |- ( ( A -> ( A -> A ) ) -> ( A -> A ) ) $=
  wff_A
  wff_1
  wff_A
  wff_2
  wff_A
  step_1
  wff_A
  step_2
  modus_ponens
$.

step_4 $p |- ( A -> ( A -> A ) ) $=
  wff_A
  wff_A
  axiom_1
$.

th1 $p |- ( A -> A ) $=
  wff_A
  wff_A
  wff_A
  wff_imp
  wff_imp
  wff_A
  wff_A
  wff_imp
  wff_A
  step_4
  wff_A
  step_3
  modus_ponens
$.

th2 $p |- ( ( ( not B ) -> B ) -> B ) $=
  wff_B
  wff_not
  wff_B
  wff_not
  wff_imp
  wff_B
  wff_not
  wff_B
  wff_imp
  wff_B
  wff_imp
  wff_B
  wff_not
  th1
  wff_B
  wff_B
  axiom_3
  modus_ponens
$.
