# Shourya Poddar
## Question 1
```bash
<C> ::= <AssignExpr>

<AssignExpr> ::= <CondExpr> | <CondExpr> = <AssignExpr>

<CondExpr> ::= <OrExpr> | <OrExpr> ? <CondExpr> : <CondExpr>

<OrExpr> ::= <OrExpr> || <AndExpr> | <AndExpr>

<AndExpr> ::= <AndExpr> && <NotExpr> | <NotExpr>

<NotExpr> ::= !<NotExpr> | <Primary>

<Primary> ::= ( <C> ) | <V> | true| false

<V> ::= x | y | z
```
## Question 2
```text
<start1>  → <stmt3> ; <start3>
<start2>  → <stmt4>
<stmt1>   → <declare2>
<stmt2>   → <assign2>
<declare1> → <type3> <var>
<type1>   → int
<type2>   → double
<assign1> → <var> = <expression3>
<expression1> → <expression4> <op> <expression5>
<expression2> → <value4>
<op>      → + | - | * | ÷
<value1>  → <var>
<value2>  → <integer>
<value3>  → <float>
```

## Question 3

## Question 4
intial code:

```text
// Precondition: n ≥ 0 and A has n elements indexed from 0
1. bound = n;

/* Outer loop invariant:
   A[bound..(n-1)] is sorted (non-decreasing)
   AND
   every element in A[bound..(n-1)] ≥ all elements in A[0..bound-1]
*/
2. while (bound > 0) {

3.    t = 0;
4.    i = 0;

     /* Inner loop invariant (given):
        "A[t] is the largest element of A[0..t]"
     */

5.    while (i < bound - 1) {
6.       if (A[i] > A[i+1]) {
7.          swap = A[i];
8.          A[i] = A[i+1];
9.          A[i+1] = swap;
10.         t = i+1;
         }
11.      i++;
     }
12.   bound = t;
}

// Postcondition: A[0] ≤ A[1] ≤ ... ≤ A[n-1]
```
## 1. Outer Loop Invariant
- **Invariant**:  
  A[bound..(n-1)] is sorted (non-decreasing) and every element in A[bound..(n-1)] ≥ every element in A[0..(bound-1)].

- **Comment**:  
  This is standard for a bubble sort variant in which the boundary `bound` shrinks from `n` down to `0`. The portion `[bound..(n-1)]` represents the sorted “tail” of the array.

---

## 2. Inner Loop Invariant
- **Given Statement**:  
  “A[t] is the largest element of A[0..t].”

- **Typical Bubble Sort**:  
  Usually, after each inner iteration, the largest element among the range is bubbled toward the end of the subarray. A more common statement might say, “After `i` steps, the largest among `A[0..i]` is at index `i`.”  
  But in the given code, `t` is set to the index of the last swap. Hence, the provided invariant may need refinement because `t` specifically tracks the position of the final swap within the inner loop, and we later set `bound = t` to skip the sorted tail.

---

## 3. Weakest Preconditions (WP) for Each Numbered Statement

Below is a concise list of the **weakest precondition** (WP) for each statement in the code:

1. **WP(1)**: `n ≥ 0 and A has n elements`
    - Ensures `bound = n` does not violate array bounds.
    - Establishes an **empty sorted segment** `[n..n−1]`.

2. **WP(2)**: The **outer loop invariant** holds with `0 ≤ bound ≤ n`.
    - This means the subarray `[bound..(n-1)]` is sorted and ≥ every element in `[0..(bound-1)]`.
    - Also ensures `bound ≥ 0` so that `while (bound > 0)` makes sense.

3. **WP(3)**: Outer loop invariant still true, `bound > 0`; setting `t=0` does not break the outer loop invariant.

4. **WP(4)**: Outer loop invariant holds, `t=0`; then `i=0` sets up for the **inner loop**.

5. **WP(5)**: The **inner loop invariant** is established:
    - “A[t] is the largest element in `A[0..t]`” (which is trivially true if `t=0`),
    - `i=0` ensures we start scanning from the beginning of the subarray.

6. **WP(6)**: Inner loop invariant is in place; `i < bound−1`.
    - This ensures valid indexing when accessing `A[i+1]`.

7. **WP(7, 8, 9)**: Same condition that “A[t] is largest in `A[0..t]`” and `i < bound−1`; each swap statement is safe and preserves the invariant once the swaps are complete.

8. **WP(10)**: If a swap happened, the largest element between `A[i]` and `A[i+1]` is now in `A[i+1]`. Hence setting `t = i+1` restores the invariant “A[t] is largest in `A[0..t]`.”

9. **WP(11)**: The inner loop invariant remains valid as `i++` just moves to the next comparison.

10. **WP(12)**: By the end of the inner loop, the subarray `[t..(bound−1)]` has had its largest elements bubbled to the right.
    - Setting `bound = t` effectively re‐establishes the **outer loop invariant** for the next pass or ensures the array is fully sorted if `t = 0`.

---

## Is WP(1) Inferred by the Code's Precondition?

### Statement 1
```cpp
1. bound = n;
```
Weakest Precondition for Statement 1

	“n ≥ 0 and A has n elements indexed from 0.”

This requirement ensures that:
-	Setting bound = n does not cause any out‐of‐range index issues,
-	The subarray [n..(n-1)] is empty (and trivially sorted).

Overall Precondition of the Code

	“n ≥ 0 and A contains n elements indexed from 0.”

Since the stated precondition of the code matches exactly what we need for the weakest precondition of Statement 1, we can conclude:

Yes—the weakest precondition for Statement 1 is logically inferred by (and is essentially the same as) the overall precondition of the code.

Therefore, the code’s stated precondition already guarantees everything needed before executing Statement 1.

---
# Question 5

We want to define a function
\\( M_{\text{state}}(\langle \text{syntax} \rangle, \sigma) \\)
that takes a **syntax tree** (for an `<assign>`, `<if>`, or `<while>`) and an **initial state** \\( \sigma \\), and returns a **new state**.

## Assumptions and Helper Mappings

- **\\( M_{\text{int}}(\langle \text{expression} \rangle, \sigma) \\)**  
  Evaluates a numeric expression in state \\( \sigma \\), returning a numeric value (or an error).

- **\\( M_{\text{boolean}}(\langle \text{condition} \rangle, \sigma) \\)**  
  Evaluates a condition in state \\( \sigma \\), returning `true` or `false` (or an error).

- **\\( M_{\text{name}}(\langle \text{var} \rangle) \\)**  
  Extracts the *string name* of the variable from the syntax tree (or an error).  
  We assume no state is required to parse the variable’s name itself.

- **AddBinding(name, value, \\( \sigma\\))**  
  Returns a **new** state in which the key `name` is bound to `value`.

- **RemoveBinding(name, \\( \sigma\\))**  
  Returns a new state in which any binding of `name` in \\( \sigma\\) is removed.  
  (We assume no errors are raised.)

- **Side Effects**  
  The problem states that *expressions and conditions can change the values of variables.*  
  This means \\( M_{\text{int}} \\) and \\( M_{\text{boolean}} \\) might return both a *value* and an *updated state* \\( \sigma' \\).  
  Below, we show a typical *monadic‐style* or *pair* notation to capture that possibility.

---

## 1. Assignment
### Syntax Rule
```bnf
<assign> → <var> = <expression>
```

We first evaluate the `<expression>` in the current state $$\(\sigma\)$$, obtaining a numeric value $$\(v\)$$ and a possibly updated state $$\(\sigma_1\)$$. Then we add a new binding of `<var>` to $$\(v\)$$ in $$\(\sigma_1\)$$.

Formally:


$$
M_{\text{state}}\bigl(\langle \text{var}\rangle = \langle \text{expression}\rangle,\;\sigma\bigr)
= \text{let}\;\bigl(v,\;\sigma_1\bigr)
= M_{\text{int}}\bigl(\langle \text{expression}\rangle,\;\sigma\bigr)
\;\text{in}\;
\text{AddBinding}\Bigl(
M_{\text{name}}\bigl(\langle \text{var}\rangle\bigr),
v,\;\sigma_1
\Bigr).
$$

If $$\(M_{\text{int}}\)$$ can produce an error, you would handle it, but typically we just assume it returns $$\((v,\sigma_1)\)$$.

---

## 2. If-Then-Else
### Syntax Rule
```bnf
<if> → if <condition> then <statement1> else <statement2>
```
Denotational Semantics

We evaluate the <condition> in state $$\( \sigma \)$$. Suppose that yields $$(b, σ_1)$$, where b is true or false, and $$\( \sigma_1 \)$$ is the possibly updated state:

$$
\begin{aligned}
M_{\text{state}}\bigl(\text{if } \langle \text{condition} \rangle \text{ then } S_1 \text{ else } S_2,\;\sigma\bigr)
&= \text{let } (b,\sigma_1)
= M_{\text{boolean}}\bigl(\langle \text{condition}\rangle,\;\sigma\bigr) \\
&\quad \text{in } \begin{cases}
M_{\text{state}}(S_1,\;\sigma_1), & \text{if } b = \text{true},\\
M_{\text{state}}(S_2,\;\sigma_1), & \text{if } b = \text{false}.
\end{cases}
\end{aligned}
$$

That is, we:
1.	Evaluate <condition>, get $$(b, σ_1)$$.
2.	If b is true, evaluate <statement1> in state $$\( \sigma_1 \)$$.
3.	Otherwise, evaluate <statement2> in state $$\( \sigma_1 \)$$.
---
## 3.While Loop

### Syntax Rule
```bnf
<while> → while <condition> <loop body>
```
Denotational Semantics

We repeatedly evaluate the `<condition>`; if it is true, we execute the `<loop body>`, then check the condition again. If it is false, we exit.

Formally:

$$
\begin{aligned}
M_{\text{state}}\bigl(\text{while } \langle \text{condition}\rangle \; S,\;\sigma\bigr)
&= \text{let }(b,\sigma_1)
= M_{\text{boolean}}\bigl(\langle \text{condition}\rangle,\;\sigma\bigr) \\
&\quad \text{in }
\begin{cases}
M_{\text{state}}\Bigl(\text{while } \langle \text{condition}\rangle \; S,\;\sigma_2\Bigr),
& \text{if } b = \text{true},\\
\sigma_1,
& \text{if } b = \text{false}.
\end{cases}\\
&\quad \text{where } \sigma_2 = M_{\text{state}}\bigl(S,\;\sigma_1\bigr).
\end{aligned}
$$

**Step-by-step meaning**:

1. Evaluate `<condition>` in \(\sigma\), producing \((b,\sigma_1)\).
2. If \(b\) is **true**:
    - Evaluate the loop body \(S\) in \(\sigma_1\), yielding new state \(\sigma_2\).
    - Re‐evaluate the entire `while` statement with \(\sigma_2\).
3. If \(b\) is **false**:
