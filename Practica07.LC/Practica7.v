(*
--- Equipo Omeguita
--- Alvarado Camacho Andrea     318064343
--- Mondragón Segoviano Alfonso 115000957
*)

(* NATURALES *)
(* 1 *)
Inductive naturales : Type := | cero : naturales | Suc : naturales ->naturales.

Fixpoint suma (x y : naturales) : naturales :=
match x with
  |cero => y
  |Suc n => Suc(suma n y)
 end.
 
Fixpoint producto (x y : naturales) : naturales :=
match x with
  |cero => cero
  |Suc n => suma(producto n y) y
 end.
 
(* 2 *)
(*Aux1. Neutro aditivo*)
Lemma NeutroSuma: forall a: naturales, a = suma a cero.
Proof.
induction a.
trivial.
simpl.
rewrite <- IHa.
trivial.
Qed.
(*Aux2. Lemma aditivo*)
Lemma Suma: forall a b: naturales, suma a (Suc b) = Suc (suma a b).
Proof.
induction a.
intros.
simpl.
trivial.
intros.
simpl.
rewrite IHa.
trivial.
Qed.
(*Aux3. Conmutatividad sobre la suma   a + b = b + a *)
Lemma Conmutatividad: forall a b: naturales, suma a b = suma b a.
Proof.
induction b.
simpl.
rewrite NeutroSuma.
trivial.
simpl.
rewrite Suma.
rewrite IHb.
trivial.
Qed.
(*Aux4. Asociatividad sobre la suma   a + (b + c) = (a + b) + c *)
Lemma AsociaUno: forall a b c: naturales, suma a (suma b c) = suma (suma a b) c.
Proof.
induction a.
trivial.
intros.
simpl.
rewrite IHa.
trivial.
Qed.
(*Aux5. Asociatividad sobre la suma   (a + b) + c = a + (b + c) *)
Lemma AsociaDos: forall a b c: naturales, suma (suma a b) c = suma a (suma b c).
Proof.
induction a.
trivial.
intros.
simpl.
rewrite IHa.
trivial.
Qed.

Theorem DistriProdSobreSuma: forall n m r: naturales,
producto n (suma m r) = suma (producto n m) (producto n r).
Proof.
induction n.
intros.
trivial.
intros.
simpl.
rewrite IHn.
remember (producto n m) as x.
remember (producto n r) as y.
rewrite Conmutatividad.
rewrite AsociaDos.
rewrite Conmutatividad.
rewrite AsociaUno.
rewrite AsociaDos.
rewrite AsociaDos.
rewrite Conmutatividad.
rewrite (Conmutatividad (y) m).
rewrite AsociaUno.
rewrite AsociaDos.
trivial.
Qed.

(* 3 *)
(* P.D. Para cualquier número n que pertenezca a los naturales, su sucesor es distinto de cero. *)
Theorem SucDistCero: forall n: naturales, Suc n <> cero.
Proof.
induction n.
intro.
discriminate.
intro.
discriminate.
Qed.

(* P.D. Para cualesquiera números n y m que pertenezcan a los naturales, si el sucesor de n es igual al sucesor de m, entonces n es igual a m. *)
(*Theorem SucIgual: forall n m: naturales, (Suc n = Suc m) -> (n = m).
Proof.
induction n.
induction m.
intros.
trivial.
intro.
rewrite IHm.
discriminate.
rewrite H.
discriminate.
intro.
intro.*)

(* LÓGICA PROPOSICIONAL *)
(* 4 *)
Variables P Q : Prop.
Proposition P1: (P /\ Q <-> P) -> (Q <-> P \/ Q).
Proof.
intros.
destruct H.
split.
intro.
right.
exact H1.
intro.
destruct H1.
apply H0.
exact H1.
exact H1.
Qed.

(* 5 *)
Variables R S T : Prop.
Proposition P2: (P -> Q -> R) /\ (P \/ S) /\ ( T -> Q) /\ (~S) -> ~R -> ~T.
Proof.
intros.
destruct H.
destruct H1.
destruct H2.
intro.
apply H0.
destruct H1.
apply H.
exact H1.
apply H2.
exact H4.
apply H.
destruct H3.
exact H1.
apply H2.
exact H4.
Qed.