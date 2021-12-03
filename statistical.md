Model Assumptions
-----------------

-   Spatial data can be defined as realizations of a stochastic process
    indexed by *Y*(*s*) ≡ {*y*(*s*), *s ∈ D*}, where D is a fixed
    subset of ℛ<sup>2</sup>.
-   The actual data can be formally represented by a collection of
    observations
    *y = {y*(*s*<sub>1</sub>), ..., *y*(*s*<sub>*n*</sub>)}, where the
    set (*s*<sub>1</sub>, ..., *s*<sub>*n*</sub>) represents the spatial
    units. Here, *y* represents the number of infected cases observed in
    each public health unit.
-   *y*<sub>*i*</sub> ∼ Poisson(*μ*<sub>*i*</sub>). Let
    *μ*<sub>*i*</sub>/*e*<sub>*i*</sub> denote the unknown infected risk
    in zone i (i = 1,2,…,n) and *e*<sub>*i*</sub> is the population size
    in zone i.
-   *l**o**g*(*μ*<sub>*i*</sub>) = *l**o**g*(*e*<sub>*i*</sub>) + *θ*<sub>*i*</sub>,
    where *θ*<sub>*i*</sub> is the log infected risk of COVID-19 in
    area i.

Defining Bayesian Spatial-temporal Models
-----------------------------------------

*y*<sub>*i**t*</sub> ∼ *P**o**i**s**s**o**n*(*μ*<sub>*i**t*</sub>)<br />
*l**o**g*(*μ*<sub>*i**t*</sub>) = *l**o**g*(*e*<sub>*i*</sub>) + *θ*<sub>*i**t*</sub><br />
*θ*<sub>*i**t*</sub> = *α + β**X + u*<sub>*i*</sub> + *v*<sub>*i*</sub> + *γ*<sub>*t*</sub> + *ϕ*<sub>*t*</sub> + *δ*<sub>*i**t*</sub><br />
(*u*<sub>*k*</sub>|*u*<sub>*i*</sub>, *k ≠ i*, *τ*<sub>*u*</sub><sup>2</sup>) ∼ *N*(∑<sub>*i*</sub>*u*<sub>*i*</sub>*ω*<sub>*k**i*</sub>/∑<sub>*i*</sub>*ω*<sub>*k**i*</sub>,*τ*<sub>*u*</sub><sup>2</sup>/∑<sub>*i*</sub>*ω*<sub>*k**i*</sub>)<br />
*v*<sub>*i*</sub> ∼ *N*(0, *τ*<sub>*v*</sub><sup>2</sup>)<br />
*γ*<sub>*t*</sub>|*γ*<sub>*t − 1</sub>, γ*<sub>*t − 2</sub> ∼ N*(2*γ*<sub>*t − 1</sub> + γ*<sub>*t − 2</sub>, σ*<sub>*γ*</sub><sup>2</sup>)<br />
*ϕ*<sub>*t*</sub> ∼ *N*(0, *σ*<sub>*ϕ*</sub><sup>2</sup>)<br />
where *y*<sub>*i*</sub> is the age-adjusted number of COVID-19 cases in
i=1,…,I areas;
*β = (β*<sub>1</sub>, *β*<sub>2</sub>, ..., *β*<sub>*p*</sub>)
represents the vector of covariate coefficients;
*X = (x*<sub>1</sub>, *x*<sub>2</sub>, ..., *x*<sub>*p*</sub>) is the
COVID-19 relevant covariate data vector. *u*<sub>*i*</sub> is a
spatially structured random effect under GMRF with *k ∼ i* referring
to neighbor regions i and j sharing a common boundary line and
*v*<sub>*i*</sub> is a spatially unstructured random effect with mean
zero and variance *τ*<sub>*v*</sub><sup>2</sup>.
*ω*<sub>*k**i*</sub> = 1 if k,i are adjacent, *ω*<sub>*k**i*</sub> = 0
otherwise. The term *γ*<sub>*t*</sub> represents the temporally
structured effect, modeled dynamically through a neighboring structure
using a second-order random walk
*γ*<sub>*t*</sub>|*γ*<sub>*t − 1</sub>, γ*<sub>*t − 2</sub> ∼ N*(2*γ*<sub>*t − 1</sub> + γ*<sub>*t − 2</sub>, σ*<sub>*γ*</sub><sup>2</sup>)
and *ϕ*<sub>*t*</sub> ∼ *N*(0, *σ*<sub>*ϕ*</sub><sup>2</sup>).

### Four Different Interaction Types

<table>
<thead>
<tr class="header">
<th>Type of sptaial-temporal interaction</th>
<th><span class="math inline"><em>R</em><sub><em>δ</em></sub></span></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Ⅰ</td>
<td><span class="math inline"><em>I</em><sub><em>s</em></sub>⨂<em>I</em><sub><em>t</em></sub></span></td>
</tr>
<tr class="even">
<td>II</td>
<td><span class="math inline"><em>I</em><sub><em>s</em></sub>⨂<em>R</em><sub><em>t</em></sub></span></td>
</tr>
<tr class="odd">
<td>III</td>
<td><span class="math inline"><em>R</em><sub><em>s</em></sub>⨂<em>I</em><sub><em>t</em></sub></span></td>
</tr>
<tr class="even">
<td>IV</td>
<td><span class="math inline"><em>R</em><sub><em>s</em></sub>⨂<em>R</em><sub><em>t</em></sub></span></td>
</tr>
</tbody>
</table>

where the identity matrices *I*<sub>*s*</sub>(*I*<sub>*t*</sub>)
correspond to the unstructured spatial (temporal) effect respectively,
whereas *R*<sub>*t*</sub>(*R*<sub>*s*</sub>) represent non-identity
matrices that correspond to a specific structured temporal (spatial)
effect.
