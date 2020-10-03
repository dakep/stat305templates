---
student: David K
student_id: 10101010
url: http://127.0.0.1/lab_04.Rmd
pubkey: MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAECEBmlRqws4b7xZSp4dhunPFrpRXbsqF9gPIsihjqOqC3lSpqnvenZmx3WD9A1+uw65vbCYfbyGIav343Y1frVg==
signature: MEYCIQCtxksHFKhvneJbyCtLrALRrhdqVt5uG0DQwb5l47rOSQIhAIH4V10Me3plx7Ubci+ZXLBBHX5cBCtuB1ww69UUOoEe;MEYCIQChEz+v4AqGA4pIssu18YdbqjKvKuC7uH+FMYnzDTA4zAIhAJlR6o7MmWjOKPBaZo7xCyhkGjcpdHdCbO8xOqkey8h9
---
# Answers
## applet_idv
```
125.3
```


## applet_n
```
100
```


## applet_p1
```
0.15
```


## applet_p2
```
0.45
```


## applet_p3
```
0.1
```


## applet_p4
```
0.3
```


## applet_sim
```
1
```


## q1a_1-generate_table
```
## Fill in the probabilities column wise.
observed_frequencies <- matrix(rmultinom(1, size = 125, prob = c(0.15, 0.45, 0.10, 0.30)), nrow = 2)
# the matrix function creates a 2 by 2 table by specifying nrow = 2
observed_frequencies
```
#### Result



<pre><code>     [,1] [,2]
[1,]   24   10
[2,]   50   41</code></pre>




## q1a_2-chi_square_test
```
## Chose the right logical indicator
observed_frequencies
chisq.test(observed_frequencies, correct = FALSE)
```
#### Result



<pre><code>     [,1] [,2]
[1,]   28   13
[2,]   43   41</code></pre>
<pre><code>
    Pearson&#39;s Chi-squared test

data:  observed_frequencies
X-squared = 3.2842, df = 1, p-value = 0.06995</code></pre>




## q2a_1-condition1-31a117b8-text
```
123
```


## q2a_2-condition1-discussion-3c0afda6-text
```
asdf
```


## q2b_1-condition2-10ea1942-text
```
123
```


## q2b_2-condition2-discussion-4312db87-text
```
asdf
```


## q2c_1-condition3-3f2dc821-text
```
123
```


## q2c_2-condition3-discussion-65891582-text
```
asdf
```


## q2d_1-condition4-1f170046-text
```
123
```


## q2d_2-condition4-discussion-50873ca4-text
```
asdf
```


## zz_group-helped_most_first
```
d
```


## zz_group-helped_most_last
```
d
```


## zz_group-member_names
```
d
```


