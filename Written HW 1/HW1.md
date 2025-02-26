# Shourya Poddar
## Question 1

## **Step 1: Configure Precedence (Separate into Nonterminals)**

We split each precedence level into its own symbol, from lowest to highest:

1. **`<A7>`** – handles assignment (`=`)  
2. **`<B7>`** – handles the ternary operator (`?:`)  
3. **`<C7>`** – handles logical OR (`||`)  
4. **`<D7>`** – handles logical AND (`&&`)  
5. **`<E7>`** – handles logical NOT (`!`)  
6. **`<G7>`** – handles parentheses, booleans, and variables  
7. **`<V7>`** – variable names

**Roughly** (before enforcing associativity):
```text
<A7> → <V7> = <A7> | <B7>
<B7> → <C7>?<B7>:<B7> | <C7>
<C7> → <C7> || <D7> | <D7>
<D7> → <D7> && <E7> | <E7>
<E7> → !<E7> | <G7>
<G7> → ( <A7> ) | true | false | <V7>
 <V7> → x | y | z
```

## **Step 2: Add Right & Left Associativities**

- **Right‐associative**: `=`, `?:`, `!`  
  - The grammar `<A7> → <V7> = <A7> | <B7>` ensures we parse `=` from the **right**.  
  - `<B7> → <C7>?<B7>:<B7> | <C7>` also parses chained ternaries from the **right**.  
  - `<E7> → !<E7> | <G7>` is already right‐associative because `!` recurses on `<E7>`.  

- **Left‐associative**: `||`, `&&`  
  - `<C7> → <C7> || <D7> | <D7>` means repeated `||` group left.  
  - `<D7> → <D7> && <E7> | <E7>` means repeated `&&` group left.

---

Putting it all together in the final unambiguous grammar:

```bnf
<A7> → <V7> = <A7>
     | <B7>

<B7> → <C7> ? <B7> : <B7>
     | <C7>

<C7> → <C7> || <D7>
     | <D7>

<D7> → <D7> && <E7>
     | <E7>

<E7> → ! <E7>
     | <G7>

<G7> → ( <A7> )
     | true
     | false
     | <V7>

<V7> → x | y | z
```
## Question 2

### Charaterizing the Atributtes 
Inherited Attributes
- Typetable: passed down to statements
- Initable: passed down to assignments and expressions

Synthesized Attributes
- type: computed up, property of expression
- typebinding: computed up when varible declaration
- initialized

### 1. Start Productions

### `<start1> → <stmt3> ; <start3>`
- `<stmt3>.typetable := <start1>.typetable`
- `<stmt3>.inittable := <start1>.inittable`
- `<start3>.typetable := <start1>.typetable  // possibly updated after <stmt3>`
- `<start3>.inittable := <start1>.inittable  // possibly updated after <stmt3>`

### `<start2> → <stmt4>`
- `<stmt4>.typetable := <start2>.typetable`
- `<stmt4>.inittable := <start2>.inittable`

---

### 2. Statement Productions

### `<stmt1> → <declare2>`
- `<declare2>.typetable := <stmt1>.typetable`
- `<declare2>.inittable := <stmt1>.inittable`
- `<stmt1>.typebinding := <declare2>.typebinding`  

### `<stmt2> → <assign2>`
- `<assign2>.typetable := <stmt2>.typetable`
- `<assign2>.inittable := <stmt2>.inittable`
- `<stmt2>.initialized := <assign2>.initialized`

---

### 3. Declaration

### `<declare2> → <type3> <var>`
- `<type3>.typetable := <declare2>.typetable`
- `<type3>.inittable := <declare2>.inittable`
- `<declare2>.typebinding := ( <var>, <type3>.type )`
- `<declare2>.initialized := ( <var>, false )`

---

### 4. Types

### `<type1> → int`
- `<type1>.type := int`

### `<type2> → double`
- `<type2>.type := double`

---

### 5. Assignment

### `<assign1> → <var> = <expression3>`
- `<expression3>.typetable := <assign1>.typetable`
- `<expression3>.inittable := <assign1>.inittable`
- `<assign1>.initialized := ( <var>, true )`

---

### 6. Expressions

### `<expression1> → <expression4> <op> <expression5>`
- `<expression4>.typetable := <expression1>.typetable`
- `<expression5>.typetable := <expression1>.typetable`
- `<expression4>.inittable := <expression1>.inittable`
- `<expression5>.inittable := <expression1>.inittable`
- `<expression1>.type := <expression4>.type`

### `<expression2> → <value4>`
- `<value4>.typetable := <expression2>.typetable`
- `<value4>.inittable := <expression2>.inittable`
- `<expression2>.type := <value4>.type`

### `<op> → + | - | * | ÷`

---

### 7. Values

### `<value1> → <var>`
- `<var>.typetable := <value1>.typetable`
- `<var>.inittable := <value1>.inittable`
- `<value1>.type := <var>.type`

### `<value2> → <integer>`
- `<value2>.type := integer`

### `<value3> → <float>`
- `<value3>.type := double`

---

### 8. Terminals

- `<var> → a legal name in the language`
- `<integer> → a base 10 representation of an integer`
- `<float> → a base 10 representation of a floating point number`

## Question 3
### (a) Type of Expression Must Match the Variable (Assignments)

**Where:** Any `<assign>` production, i.e. `<assign1> → <var> = <expression3>`.

**Check:** Compare the variable’s declared type to the expression’s type. Formally:
```pseudo
if <assign1>.typetable(<var>) != <expression3>.type then
   error("Type mismatch in assignment")
```

### (b) A Variable Must Be Declared Before Use

**Where**: Anywhere `<var>` appears
- At `<assign1> → <var> = <expression3>`:

 ```pseudo
if <assign1>.typetable(<var>) == error then
   error("Use of undeclared variable in assignment")
```
- At `<value1> → <var>`:
```pseudo
if <value1>.typetable(<var>) == error then
   error("Use of undeclared variable in expression")
```
### (c) A Variable Must Be Initialized Before Use

**Where**: Any place `<var>` is used but not assigned.
- 	At `<value1> → <var>`:
```pseudo
if <value1>.inittable(<var>) == false then
   error("Use of uninitialized variable")
```

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
## **A1. Outer Loop Invariant**

> **Claimed Invariant**:  
> “`A[bound..(n-1)]` is sorted (non-decreasing), and every element in that segment is ≥ every element in `A[0..(bound-1)]`.”

### 1. Is It Correct?
Yes. This is a **valid invariant** for a bubble sort variant in which `bound` shrinks from `n` down to `0`. The subarray `A[bound..(n-1)]` is the “sorted tail,” containing the largest elements in non‐decreasing order, each of which is no less than any element in `A[0..(bound-1)]`.

### 2. Proof of Correctness

1. **Initialization**  
   - At the very start, `bound` is set to `n`, so `A[n..(n-1)]` is an empty subarray (trivially sorted). An empty set is also trivially “≥” everything else. So the invariant holds before the first iteration of the outer loop.

2. **Maintenance**  
   - Assume the invariant holds at the start of some outer loop iteration (i.e., `A[bound..(n-1)]` is sorted and ≥ everything in `A[0..(bound-1)]`).  
   - The inner loop “bubbles” a largest element to the front of that sorted tail. By the time the inner loop finishes, we update `bound` (e.g., `bound = t`). Because the newly bubbled position becomes the new boundary, the subarray `A[bound..(n-1)]` remains sorted and remains ≥ all elements in `A[0..(bound-1)]`. Thus the invariant is preserved.

3. **Termination**  
   - Eventually, `bound` reaches 0 or 1. If `bound = 0`, then the entire array is now the “sorted tail,” so the entire array `A[0..(n-1)]` is sorted in non‐decreasing order. This meets the sort’s desired postcondition.

Hence, the outer loop invariant is **correct**.

---

## **A2. Inner Loop Invariant**

> **Claimed Invariant**:  
> “`A[t]` is the largest element of `A[0..t]`.”

### 1. Is It Correct?
It is **partly correct** but **needs refinement** for a typical bubble sort. The code keeps track of the index of the **last swap** in `t`. If, whenever `A[i] > A[i+1]`, we swap and then set `t = i+1`, it can mean “the current largest encountered so far is now at `i+1`.”  

In the given code, `t` indeed tracks the position of the last (most recent) swap, so the statement “`A[t]` is the largest element of `A[0..t]`” is not the conventional statement but can still be **true** if the code consistently updates `t` to point to the newly placed larger element. One might prefer a more explicit wording:

> **Refined**:  
> “Just after each inner‐loop iteration, the index `t` refers to the position of the rightmost swap so far, meaning `A[t]` is the largest element in `A[0..t]`.”

### 2. Proof of Correctness (Refined Statement)

1. **Initialization**  
   - At the beginning of the inner loop, `t` is set to 0, and `i=0`. For `t=0`, it is trivially true that `A[t]` is the largest element in `A[0..0]`.

2. **Maintenance**  
   - Suppose at the start of some inner loop iteration (with index `i`) the statement “`A[t]` is largest in `A[0..t]`” holds.  
   - If we do not swap at `i`, no changes to `A[t]` or `t` occur, so the invariant is unchanged.  
   - If we swap `A[i]` with `A[i+1]` (because `A[i] > A[i+1]`), then we set `t = i+1`. Now the bigger value is at `i+1`, so `A[t]` is the largest among `A[0..t]`.  
   - Thus the invariant holds at the end of each inner iteration.

3. **Termination**  
   - When the inner loop exits, we have the position of the last (rightmost) swap in `t`. This means everything from `t` onward is “bubbled,” and we then set `bound = t`. The “sorted tail” shrinks accordingly, consistent with the outer loop invariant.

Thus, with that **slightly refined** statement, we can show correctness by initialization, maintenance, and termination.

---

## B. Weakest Preconditions (WP) for Each Numbered Statement

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

## C. Is WP(1) Inferred by the Code's Precondition?

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

$$
M_{\text{state}}\bigl(\langle \text{syntax} \rangle, \sigma\bigr)
$$

that takes a **syntax tree** (for an `<assign>`, `<if>`, or `<while>`) and an **initial state** 
$$\sigma$$
, and returns a **new state**.

---

## Assumptions and Helper Mappings

- $$M_{\text{int}}\bigl(\langle \text{expression} \rangle, \sigma\bigr)$$
  Evaluates a numeric expression in state 
  $$\sigma$$
  , returning a numeric value (or an error).

- $$M_{\text{boolean}}\bigl(\langle \text{condition} \rangle, \sigma\bigr)$$ 
  Evaluates a condition in state 
  $$\sigma$$
  ,returning `true` or `false`.

- $$M_{\text{name}}\bigl(\langle \text{var} \rangle\bigr)$$
  Extracts the *string name* of the variable from the syntax tree.  
  We assume no state is required to parse the variable’s name.

- $$\text{AddBinding}(\text{name}, \text{value}, \sigma)$$ 
  Returns a **new** state in which the key `name` is bound to `value`.

- $$\text{RemoveBinding}(\text{name}, \sigma)$$ 
  Returns a new state in which any binding of `name` in 
  $$\sigma $$
  is removed.

---

## Side Effects

The problem states that *expressions and conditions can change the values of variables.*  
Hence, both 
$$M_{\text{int}}$$ and 
$$M_{\text{boolean}}$$ might return **a pair** $$(\text{value}, \sigma')$$, meaning “the computed value and the updated state.”  

---

## A. Assignment
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

## B. If-Then-Else
### Syntax Rule
```bnf
<if> → if <condition> then <statement1> else <statement2>
```
Denotational Semantics



We evaluate the <condition> in state $\( \sigma \)$. Suppose that yields $$(b, σ_1)$$, where b is true or false, and $$\( \sigma_1 \)$$ is the possibly updated state:

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
## C.While Loop

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

1. Evaluate `<condition>` in $$\(\sigma\)$$, producing $$\((b,\sigma_1)\)$$.
2. If \(b\) is **true**:
    - Evaluate the loop body $$\(S\)$$ in $$\(\sigma_1\)$$, yielding new state $$\(\sigma_2\)$$.
    - Re‐evaluate the entire `while` statement with $$\(\sigma_2\)$$.
3. If $$\(b\)$$ is **false**:
