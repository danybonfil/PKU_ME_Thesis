## Pre-task Demographic Survey ########################

l: pre_task
t: radio
q: <b><font color='red'>Pre-Task</font></b>: This notice concerns <b>your payment.</b><br>
<ul>
<li> You are required to complete pre-task questions.
</ul>
- Continue

random: begin

l: age
t: radio
q: What is your age?
- Under 18
- 18 to 24
- 25 to 34
- 35 to 44
- 45 to 54
- 55 to 64
- 65 or older

l: education
t: radio
q: What is the highest level of school you have completed?
- Less than a high school diploma
- High school or equivalent
- Bachelor's Degree (e.g. BA, BS)
- Master's Degree or higher (e.g. MA, MS, PHD)

l: income
t: radio
q: What is your personal yearly income (USD)?
- Below $5K
- $5K to $10K
- $10K to $20K
- $20K to $40K
- $40K to $60K
- $60K to $80K
- Over $80K

l: religion
t: radio
q: Do you practise a religion, and if so, which one?
- Yes, I practice (e.g. pray) every day
- Yes, I practice (e.g. pray) at least once a week
- No, I do not practice any religion

l: mturkexp
t: radio
q: How long have you worked on MTurk?
- 0 to 6 months
- 7 to 12 months
- More than 12 months

l: mturkappr
t: radio
q: What is your approval rate on MTurk?
- Below 95%
- 95% to 98%
- Above 98%

l: mturkhits
t: radio
q: How many HITs have you completed on MTurk?
- 0 to 999
- 1,000 to 4,999
- Above 5,000

random: end

l: gender
t: radio
q: What is your gender?
- Male
- Female
- Other (Please Specify)

l:
t: jump
- if $gender != 3 then goto location

l: OtherGender
t: textline
q: Type your gender
- Please indicate your gender

l: location
t: radio
q: Where do you live?
- Brazil
- Europe
- India
- USA
- Other (Please Specify)

l:
t: jump
- if $location != 5 then goto ethnicity

l: OtherLocation
t: textline
q: Type your location
- Please indicate your location

l: ethnicity
q: How would you best describe your ethnic origin?
t: radio
- White
- Mixed
- Asian or Asian British
- Black or Black British
- Chinese
- Other ethnic group (Please Specify)

l:
t: jump
- if $ethnicity != 6 then goto political

l: OtherEthnicity
t: textline
q: Type your ethnicity
- Please indicate your ethnicity

l: political
q: What is your political orientation?
t: radio
- Left (Democrat or equivalent)
- Right (Republican or equivalent)
- No preference


## be aware notice and treatment randomization ########################


l: be_aware
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
Make sure you understand the following:
<ul>
<li> This HIT consist of 3 games, you <b>must complete all to qualify for payment</b>.
</ul>
- Continue

l: random_treat
t: set
- random from 1 2 3 4 5 6 7 8 9 10 11 12 13 14

l:
t: jump
- if $random_treat == 1 then goto abs1_q2
- if $random_treat == 2 then goto abs2_q2
- if $random_treat == 3 then goto abs3_q2
- if $random_treat == 4 then goto abs4_q2
- if $random_treat == 5 then goto inc1_q2
- if $random_treat == 6 then goto inc2_q2
- if $random_treat == 7 then goto inc3_q2
- if $random_treat == 8 then goto dec1_q2
- if $random_treat == 9 then goto dec2_q2
- if $random_treat == 10 then goto dec3_q2
- if $random_treat == 11 then goto nb_q2
- if $random_treat == 12 then goto nb1_q2
- if $random_treat == 13 then goto nb2_q2
- if $random_treat == 14 then goto nb3_q2

## no bonus treatment1 ########################

l: nb_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
The available budget for this HIT is $0.03, this will be <b>added to your payment. </b><br>
- Continue

l:
t: jump
- if $nb_q2 == 1 then goto random_game_n11

l: nb1_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
The available budget for this HIT is $0.06, this will be <b> added to your payment. </b><br>
- Continue

l:
t: jump
- if $nb1_q2 == 1 then goto random_game_n11

l: nb2_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
The available budget for this HIT is $0.09, this will be <b> added to your payment. </b><br>
- Continue

l:
t: jump
- if $nb2_q2 == 1 then goto random_game_n11

l: nb3_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
The available budget for this HIT is $0.12, this will be <b> added to your payment. </b><br>
- Continue

l:
t: jump
- if $nb3_q2 == 1 then goto random_game_n11

### Rand

l: random_game_n11
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_n11 == 1 then goto game1_n11
- if $random_game_n11 == 2 then goto game2_n11
- if $random_game_n11 == 3 then goto game3_n11

l: game1_n11
t: experiment
- n1nbackt

l:
t: jump
- if $random_game_n11 == 1 then goto n1_q3

l: game2_n11
t: experiment
- n1stroopnp

l:
t: jump
- if $random_game_n11 == 2 then goto n1_q3

l: game3_n11
t: experiment
- n1iowagt

l:
t: jump
- if $random_game_n11 == 3 then goto n1_q3

l: n1_q3
t: radio
q: Game 2.<br>
- Continue

l: random_game_n12
t: set
- if $random_game_n11 == 1 then 2
- if $random_game_n11 == 2 then 3
- if $random_game_n11 == 3 then 1

l:
t: jump
- if $random_game_n12 == 1 then goto game1_n12
- if $random_game_n12 == 2 then goto game2_n12
- if $random_game_n12 == 3 then goto game3_n12

l: game1_n12
t: experiment
- n1nbackt1

l:
t: jump
- if $random_game_n12 == 1 then goto n1_q4

l: game2_n12
t: experiment
- n1stroopnp1

l:
t: jump
- if $random_game_n12 == 2 then goto n1_q4

l: game3_n12
t: experiment
- n1iowagt1

l:
t: jump
- if $random_game_n12 == 3 then goto n1_q4

l: n1_q4
t: radio
q: Game 3.<br>
- Continue

l: random_game_n13
t: set
- if $random_game_n11 == 1 or $random_game_n12 == 2 then 3
- if $random_game_n11 == 2 or $random_game_n12 == 3 then 1
- if $random_game_n11 == 3 or $random_game_n12 == 1 then 2

l:
t: jump
- if $random_game_n13 == 1 then goto game1_n13
- if $random_game_n13 == 2 then goto game2_n13
- if $random_game_n13 == 3 then goto game3_n13

l: game1_n13
t: experiment
- n1nbackt2

l:
t: jump
- if $random_game_n13 == 1 then goto endquestion

l: game2_n13
t: experiment
- n1stroopnp2

l:
t: jump
- if $random_game_n13 == 2 then goto endquestion

l: game3_n13
t: experiment
- n1iowagt2

l:
t: jump
- if $random_game_n13 == 3 then goto endquestion

## No bonus 1 endsurvey ########################


## survey abs1 treatment1abs1 ########################

l: abs1_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.01</b>.<br>
- Continue

l: random_game_a11
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_a11 == 1 then goto game1_a11
- if $random_game_a11 == 2 then goto game2_a11
- if $random_game_a11 == 3 then goto game3_a11

l: game1_a11
t: experiment
- a1nbackt

l:
t: jump
- if $random_game_a11 == 1 then goto abs1_q3

l: game2_a11
t: experiment
- a1stroopnp

l:
t: jump
- if $random_game_a11 == 2 then goto abs1_q3

l: game3_a11
t: experiment
- a1iowagt

l:
t: jump
- if $random_game_a11 == 3 then goto abs1_q3

l: abs1_q3
t: radio
q: The available bonus for this Game is <b>$0.01. </b><br>
- Continue

l: random_game_a12
t: set
- if $random_game_a11 == 1 then 2
- if $random_game_a11 == 2 then 3
- if $random_game_a11 == 3 then 1

l:
t: jump
- if $random_game_a12 == 1 then goto game1_a12
- if $random_game_a12 == 2 then goto game2_a12
- if $random_game_a12 == 3 then goto game3_a12

l: game1_a12
t: experiment
- a1nbackt1

l:
t: jump
- if $random_game_a12 == 1 then goto abs1_q4

l: game2_a12
t: experiment
- a1stroopnp1

l:
t: jump
- if $random_game_a12 == 2 then goto abs1_q4

l: game3_a12
t: experiment
- a1iowagt1

l:
t: jump
- if $random_game_a12 == 3 then goto abs1_q4

l: abs1_q4
t: radio
q: The available bonus for this Game is <b>$0.01. </b><br>
- Continue

l: random_game_a13
t: set
- if $random_game_a11 == 1 or $random_game_a12 == 2 then 3
- if $random_game_a11 == 2 or $random_game_a12 == 3 then 1
- if $random_game_a11 == 3 or $random_game_a12 == 1 then 2

l:
t: jump
- if $random_game_a13 == 1 then goto game1_a13
- if $random_game_a13 == 2 then goto game2_a13
- if $random_game_a13 == 3 then goto game3_a13

l: game1_a13
t: experiment
- a1nbackt2

l:
t: jump
- if $random_game_a13 == 1 then goto endquestion

l: game2_a13
t: experiment
- a1stroopnp2

l:
t: jump
- if $random_game_a13 == 2 then goto endquestion

l: game3_a13
t: experiment
- a1iowagt2

l:
t: jump
- if $random_game_a13 == 3 then goto endquestion

## survey abs1 endsurvey ########################


## survey abs2 treatment1abs2 ########################

l: abs2_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.02. </b><br>
- Continue

l: random_game_a21
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_a21 == 1 then goto game1_a21
- if $random_game_a21 == 2 then goto game2_a21
- if $random_game_a21 == 3 then goto game3_a21

l: game1_a21
t: experiment
- a2nbackt

l:
t: jump
- if $random_game_a21 == 1 then goto abs2_q3

l: game2_a21
t: experiment
- a2stroopnp

l:
t: jump
- if $random_game_a21 == 2 then goto abs2_q3

l: game3_a21
t: experiment
- a2iowagt

l:
t: jump
- if $random_game_a21 == 3 then goto abs2_q3

l: abs2_q3
t: radio
q: The available bonus for this Game is <b>$0.02. </b><br>
- Continue

l: random_game_a22
t: set
- if $random_game_a21 == 1 then 2
- if $random_game_a21 == 2 then 3
- if $random_game_a21 == 3 then 1

l:
t: jump
- if $random_game_a22 == 1 then goto game1_a22
- if $random_game_a22 == 2 then goto game2_a22
- if $random_game_a22 == 3 then goto game3_a22

l: game1_a22
t: experiment
- a2nbackt1

l:
t: jump
- if $random_game_a22 == 1 then goto abs2_q4

l: game2_a22
t: experiment
- a2stroopnp1

l:
t: jump
- if $random_game_a22 == 2 then goto abs2_q4

l: game3_a22
t: experiment
- a2iowagt1

l:
t: jump
- if $random_game_a22 == 3 then goto abs2_q4

l: abs2_q4
t: radio
q: The available bonus for this Game is <b>$0.02. </b><br>
- Continue

l: random_game_a23
t: set
- if $random_game_a21 == 1 or $random_game_a22 == 2 then 3
- if $random_game_a21 == 2 or $random_game_a22 == 3 then 1
- if $random_game_a21 == 3 or $random_game_a22 == 1 then 2

l:
t: jump
- if $random_game_a23 == 1 then goto game1_a23
- if $random_game_a23 == 2 then goto game2_a23
- if $random_game_a23 == 3 then goto game3_a23

l: game1_a23
t: experiment
- a2nbackt2

l:
t: jump
- if $random_game_a23 == 1 then goto endquestion

l: game2_a23
t: experiment
- a2stroopnp2

l:
t: jump
- if $random_game_a23 == 2 then goto endquestion

l: game3_a23
t: experiment
- a2iowagt2

l:
t: jump
- if $random_game_a23 == 3 then goto endquestion

## survey abs2 endsurvey ########################


## survey abs3 treatment1abs3 ########################

l: abs3_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.03. </b><br>
- Continue

l: random_game_a31
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_a31 == 1 then goto game1_a31
- if $random_game_a31 == 2 then goto game2_a31
- if $random_game_a31 == 3 then goto game3_a31

l: game1_a31
t: experiment
- a3nbackt

l:
t: jump
- if $random_game_a31 == 1 then goto abs3_q3

l: game2_a31
t: experiment
- a3stroopnp

l:
t: jump
- if $random_game_a31 == 2 then goto abs3_q3

l: game3_a31
t: experiment
- a3iowagt

l:
t: jump
- if $random_game_a31 == 3 then goto abs3_q3

l: abs3_q3
t: radio
q: The available bonus for this Game is <b>$0.03. </b><br>
- Continue

l: random_game_a32
t: set
- if $random_game_a31 == 1 then 2
- if $random_game_a31 == 2 then 3
- if $random_game_a31 == 3 then 1

l:
t: jump
- if $random_game_a32 == 1 then goto game1_a32
- if $random_game_a32 == 2 then goto game2_a32
- if $random_game_a32 == 3 then goto game3_a32

l: game1_a32
t: experiment
- a3nbackt1

l:
t: jump
- if $random_game_a32 == 1 then goto abs3_q4

l: game2_a32
t: experiment
- a3stroopnp1

l:
t: jump
- if $random_game_a32 == 2 then goto abs3_q4

l: game3_a32
t: experiment
- a3iowagt1

l:
t: jump
- if $random_game_a32 == 3 then goto abs3_q4

l: abs3_q4
t: radio
q: The available bonus for this Game is <b>$0.03. </b><br>
- Continue

l: random_game_a33
t: set
- if $random_game_a31 == 1 or $random_game_a32 == 2 then 3
- if $random_game_a31 == 2 or $random_game_a32 == 3 then 1
- if $random_game_a31 == 3 or $random_game_a32 == 1 then 2

l:
t: jump
- if $random_game_a33 == 1 then goto game1_a33
- if $random_game_a33 == 2 then goto game2_a33
- if $random_game_a33 == 3 then goto game3_a33

l: game1_a33
t: experiment
- a3nbackt2

l:
t: jump
- if $random_game_a33 == 1 then goto endquestion

l: game2_a33
t: experiment
- a3stroopnp2

l:
t: jump
- if $random_game_a33 == 2 then goto endquestion

l: game3_a33
t: experiment
- a3iowagt2

l:
t: jump
- if $random_game_a33 == 3 then goto endquestion

## survey abs3 endsurvey ########################


## survey abs4 treatment1abs4 ########################

l: abs4_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.04. </b><br>
- Continue

l: random_game_a41
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_a41 == 1 then goto game1_a41
- if $random_game_a41 == 2 then goto game2_a41
- if $random_game_a41 == 3 then goto game3_a41

l: game1_a41
t: experiment
- a4nbackt

l:
t: jump
- if $random_game_a41 == 1 then goto abs4_q3

l: game2_a41
t: experiment
- a4stroopnp

l:
t: jump
- if $random_game_a41 == 2 then goto abs4_q3

l: game3_a41
t: experiment
- a4iowagt

l:
t: jump
- if $random_game_a41 == 3 then goto abs4_q3

l: abs4_q3
t: radio
q: The available bonus for this Game is <b>$0.04. </b><br>
- Continue

l: random_game_a42
t: set
- if $random_game_a41 == 1 then 2
- if $random_game_a41 == 2 then 3
- if $random_game_a41 == 3 then 1

l:
t: jump
- if $random_game_a42 == 1 then goto game1_a42
- if $random_game_a42 == 2 then goto game2_a42
- if $random_game_a42 == 3 then goto game3_a42

l: game1_a42
t: experiment
- a4nbackt1

l:
t: jump
- if $random_game_a42 == 1 then goto abs4_q4

l: game2_a42
t: experiment
- a4stroopnp1

l:
t: jump
- if $random_game_a42 == 2 then goto abs4_q4

l: game3_a42
t: experiment
- a4iowagt1

l:
t: jump
- if $random_game_a42 == 3 then goto abs4_q4

l: abs4_q4
t: radio
q: The available bonus for this Game is <b>$0.04. </b><br>
- Continue

l: random_game_a43
t: set
- if $random_game_a41 == 1 or $random_game_a42 == 2 then 3
- if $random_game_a41 == 2 or $random_game_a42 == 3 then 1
- if $random_game_a41 == 3 or $random_game_a42 == 1 then 2

l:
t: jump
- if $random_game_a43 == 1 then goto game1_a43
- if $random_game_a43 == 2 then goto game2_a43
- if $random_game_a43 == 3 then goto game3_a43

l: game1_a43
t: experiment
- a4nbackt2

l:
t: jump
- if $random_game_a43 == 1 then goto endquestion

l: game2_a43
t: experiment
- a4stroopnp2

l:
t: jump
- if $random_game_a43 == 2 then goto endquestion

l: game3_a43
t: experiment
- a4iowagt2

l:
t: jump
- if $random_game_a43 == 3 then goto endquestion

## survey abs4 endsurvey ########################


## survey i1 treatment1inc1 ########################

l: inc1_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.01. </b>.<br>
- Continue

l: random_game_i11
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_i11 == 1 then goto game1_i11
- if $random_game_i11 == 2 then goto game2_i11
- if $random_game_i11 == 3 then goto game3_i11

l: game1_i11
t: experiment
- i1nbackt

l:
t: jump
- if $random_game_i11 == 1 then goto inc1_q3

l: game2_i11
t: experiment
- i1stroopnp

l:
t: jump
- if $random_game_i11 == 2 then goto inc1_q3

l: game3_i11
t: experiment
- i1iowagt

l:
t: jump
- if $random_game_i11 == 3 then goto inc1_q3

l: inc1_q3
t: radio
q: The available bonus for this Game is <b>$0.02. </b><br>
- Continue

l: random_game_i12
t: set
- if $random_game_i11 == 1 then 2
- if $random_game_i11 == 2 then 3
- if $random_game_i11 == 3 then 1

l:
t: jump
- if $random_game_i12 == 1 then goto game1_i12
- if $random_game_i12 == 2 then goto game2_i12
- if $random_game_i12 == 3 then goto game3_i12

l: game1_i12
t: experiment
- i1nbackt1

l:
t: jump
- if $random_game_i12 == 1 then goto inc1_q4

l: game2_i12
t: experiment
- i1stroopnp1

l:
t: jump
- if $random_game_i12 == 2 then goto inc1_q4

l: game3_i12
t: experiment
- i1iowagt1

l:
t: jump
- if $random_game_i12 == 3 then goto inc1_q4

l: inc1_q4
t: radio
q: The available bonus for this Game is <b>$0.03. </b><br>
- Continue

l: random_game_i13
t: set
- if $random_game_i11 == 1 or $random_game_i12 == 2 then 3
- if $random_game_i11 == 2 or $random_game_i12 == 3 then 1
- if $random_game_i11 == 3 or $random_game_i12 == 1 then 2

l:
t: jump
- if $random_game_i13 == 1 then goto game1_i13
- if $random_game_i13 == 2 then goto game2_i13
- if $random_game_i13 == 3 then goto game3_i13

l: game1_i13
t: experiment
- i1nbackt2

l:
t: jump
- if $random_game_i13 == 1 then goto endquestion

l: game2_i13
t: experiment
- i1stroopnp2

l:
t: jump
- if $random_game_i13 == 2 then goto endquestion

l: game3_i13
t: experiment
- i1iowagt2

l:
t: jump
- if $random_game_i13 == 3 then goto endquestion

## survey inc1 endsurvey ########################


## survey i2 treatment1inc2 ########################

l: inc2_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.02. </b>
- Continue

l: random_game_i21
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_i21 == 1 then goto game1_i21
- if $random_game_i21 == 2 then goto game2_i21
- if $random_game_i21 == 3 then goto game3_i21

l: game1_i21
t: experiment
- i2nbackt

l:
t: jump
- if $random_game_i21 == 1 then goto inc2_q3

l: game2_i21
t: experiment
- i2stroopnp

l:
t: jump
- if $random_game_i21 == 2 then goto inc2_q3

l: game3_i21
t: experiment
- i2iowagt

l:
t: jump
- if $random_game_i21 == 3 then goto inc2_q3

l: inc2_q3
t: radio
q: The available bonus for this Game is <b>$0.03. </b><br>
- Continue

l: random_game_i22
t: set
- if $random_game_i21 == 1 then 2
- if $random_game_i21 == 2 then 3
- if $random_game_i21 == 3 then 1

l:
t: jump
- if $random_game_i22 == 1 then goto game1_i22
- if $random_game_i22 == 2 then goto game2_i22
- if $random_game_i22 == 3 then goto game3_i22

l: game1_i22
t: experiment
- i2nbackt1

l:
t: jump
- if $random_game_i22 == 1 then goto inc2_q4

l: game2_i22
t: experiment
- i2stroopnp1

l:
t: jump
- if $random_game_i22 == 2 then goto inc2_q4

l: game3_i22
t: experiment
- i2iowagt1

l:
t: jump
- if $random_game_i22 == 3 then goto inc2_q4

l: inc2_q4
t: radio
q: The available bonus for this Game is <b>$0.04. </b><br>
- Continue

l: random_game_i23
t: set
- if $random_game_i21 == 1 or $random_game_i22 == 2 then 3
- if $random_game_i21 == 2 or $random_game_i22 == 3 then 1
- if $random_game_i21 == 3 or $random_game_i22 == 1 then 2

l:
t: jump
- if $random_game_i23 == 1 then goto game1_i23
- if $random_game_i23 == 2 then goto game2_i23
- if $random_game_i23 == 3 then goto game3_i23

l: game1_i23
t: experiment
- i2nbackt2

l:
t: jump
- if $random_game_i23 == 1 then goto endquestion

l: game2_i23
t: experiment
- i2stroopnp2

l:
t: jump
- if $random_game_i23 == 2 then goto endquestion

l: game3_i23
t: experiment
- i2iowagt2

l:
t: jump
- if $random_game_i23 == 3 then goto endquestion

## survey inc3 endsurvey ########################


## survey i3 treatment1inc3 ########################

l: inc3_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.03. </b>
- Continue

l: random_game_i31
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_i31 == 1 then goto game1_i31
- if $random_game_i31 == 2 then goto game2_i31
- if $random_game_i31 == 3 then goto game3_i31

l: game1_i31
t: experiment
- i3nbackt

l:
t: jump
- if $random_game_i31 == 1 then goto inc3_q3

l: game2_i31
t: experiment
- i3stroopnp

l:
t: jump
- if $random_game_i31 == 2 then goto inc3_q3

l: game3_i31
t: experiment
- i3iowagt

l:
t: jump
- if $random_game_i31 == 3 then goto inc3_q3

l: inc3_q3
t: radio
q: The available bonus for this Game is <b>$0.04. </b><br>
- Continue

l: random_game_i32
t: set
- if $random_game_i31 == 1 then 2
- if $random_game_i31 == 2 then 3
- if $random_game_i31 == 3 then 1

l:
t: jump
- if $random_game_i32 == 1 then goto game1_i32
- if $random_game_i32 == 2 then goto game2_i32
- if $random_game_i32 == 3 then goto game3_i32

l: game1_i32
t: experiment
- i3nbackt1

l:
t: jump
- if $random_game_i32 == 1 then goto inc3_q4

l: game2_i32
t: experiment
- i3stroopnp1

l:
t: jump
- if $random_game_i32 == 2 then goto inc3_q4

l: game3_i32
t: experiment
- i3iowagt1

l:
t: jump
- if $random_game_i32 == 3 then goto inc3_q4

l: inc3_q4
t: radio
q: The available bonus for this Game is <b>$0.05. </b><br>
- Continue

l: random_game_i33
t: set
- if $random_game_i31 == 1 or $random_game_i32 == 2 then 3
- if $random_game_i31 == 2 or $random_game_i32 == 3 then 1
- if $random_game_i31 == 3 or $random_game_i32 == 1 then 2

l:
t: jump
- if $random_game_i33 == 1 then goto game1_i33
- if $random_game_i33 == 2 then goto game2_i33
- if $random_game_i33 == 3 then goto game3_i33

l: game1_i33
t: experiment
- i3nbackt2

l:
t: jump
- if $random_game_i33 == 1 then goto endquestion

l: game2_i33
t: experiment
- i3stroopnp2

l:
t: jump
- if $random_game_i33 == 2 then goto endquestion

l: game3_i33
t: experiment
- i3iowagt2

l:
t: jump
- if $random_game_i33 == 3 then goto endquestion

## survey inc3 endsurvey ########################


## survey d1 treatment1dec1 ########################

l: dec1_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.03. </b>
- Continue

l: random_game_d11
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_d11 == 1 then goto game1_d11
- if $random_game_d11 == 2 then goto game2_d11
- if $random_game_d11 == 3 then goto game3_d11

l: game1_d11
t: experiment
- d1nbackt

l:
t: jump
- if $random_game_d11 == 1 then goto dec1_q3

l: game2_d11
t: experiment
- d1stroopnp

l:
t: jump
- if $random_game_d11 == 2 then goto dec1_q3

l: game3_d11
t: experiment
- d1iowagt

l:
t: jump
- if $random_game_d11 == 3 then goto dec1_q3

l: dec1_q3
t: radio
q: The available bonus for this Game is <b>$0.02. </b><br>
- Continue

l: random_game_d12
t: set
- if $random_game_d11 == 1 then 2
- if $random_game_d11 == 2 then 3
- if $random_game_d11 == 3 then 1

l:
t: jump
- if $random_game_d12 == 1 then goto game1_d12
- if $random_game_d12 == 2 then goto game2_d12
- if $random_game_d12 == 3 then goto game3_d12

l: game1_d12
t: experiment
- d1nbackt1

l:
t: jump
- if $random_game_d12 == 1 then goto dec1_q4

l: game2_d12
t: experiment
- d1stroopnp1

l:
t: jump
- if $random_game_d12 == 2 then goto dec1_q4

l: game3_d12
t: experiment
- d1iowagt1

l:
t: jump
- if $random_game_d12 == 3 then goto dec1_q4

l: dec1_q4
t: radio
q: The available bonus for this Game is <b>$0.01. </b><br>
- Continue

l: random_game_d13
t: set
- if $random_game_d11 == 1 or $random_game_d12 == 2 then 3
- if $random_game_d11 == 2 or $random_game_d12 == 3 then 1
- if $random_game_d11 == 3 or $random_game_d12 == 1 then 2

l:
t: jump
- if $random_game_d13 == 1 then goto game1_d13
- if $random_game_d13 == 2 then goto game2_d13
- if $random_game_d13 == 3 then goto game3_d13

l: game1_d13
t: experiment
- d1nbackt2

l:
t: jump
- if $random_game_d13 == 1 then goto endquestion

l: game2_d13
t: experiment
- d1stroopnp2

l:
t: jump
- if $random_game_d13 == 2 then goto endquestion

l: game3_d13
t: experiment
- d1iowagt2

l:
t: jump
- if $random_game_d13 == 3 then goto endquestion

## survey dec1 endsurvey ########################


## survey d2 treatment1dec2 ########################

l: dec2_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.04. </b>
- Continue

l: random_game_d21
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_d21 == 1 then goto game1_d21
- if $random_game_d21 == 2 then goto game2_d21
- if $random_game_d21 == 3 then goto game3_d21

l: game1_d21
t: experiment
- d2nbackt

l:
t: jump
- if $random_game_d21 == 1 then goto dec2_q3

l: game2_d21
t: experiment
- d2stroopnp

l:
t: jump
- if $random_game_d21 == 2 then goto dec2_q3

l: game3_d21
t: experiment
- d2iowagt

l:
t: jump
- if $random_game_d21 == 3 then goto dec2_q3

l: dec2_q3
t: radio
q: The available bonus for this Game is <b>$0.03. </b><br>
- Continue

l: random_game_d22
t: set
- if $random_game_d21 == 1 then 2
- if $random_game_d21 == 2 then 3
- if $random_game_d21 == 3 then 1

l:
t: jump
- if $random_game_d22 == 1 then goto game1_d22
- if $random_game_d22 == 2 then goto game2_d22
- if $random_game_d22 == 3 then goto game3_d22

l: game1_d22
t: experiment
- d2nbackt1

l:
t: jump
- if $random_game_d22 == 1 then goto dec2_q4

l: game2_d22
t: experiment
- d2stroopnp1

l:
t: jump
- if $random_game_d22 == 2 then goto dec2_q4

l: game3_d22
t: experiment
- d2iowagt1

l:
t: jump
- if $random_game_d22 == 3 then goto dec2_q4

l: dec2_q4
t: radio
q: The available bonus for this Game is <b>$0.02. </b><br>
- Continue

l: random_game_d23
t: set
- if $random_game_d21 == 1 or $random_game_d22 == 2 then 3
- if $random_game_d21 == 2 or $random_game_d22 == 3 then 1
- if $random_game_d21 == 3 or $random_game_d22 == 1 then 2

l:
t: jump
- if $random_game_d23 == 1 then goto game1_d23
- if $random_game_d23 == 2 then goto game2_d23
- if $random_game_d23 == 3 then goto game3_d23

l: game1_d23
t: experiment
- d2nbackt2

l:
t: jump
- if $random_game_d23 == 1 then goto endquestion

l: game2_d23
t: experiment
- d2stroopnp2

l:
t: jump
- if $random_game_d23 == 2 then goto endquestion

l: game3_d23
t: experiment
- d2iowagt2

l:
t: jump
- if $random_game_d23 == 3 then goto endquestion

## survey dec2 endsurvey ########################


## survey d3 treatment1dec3 ########################

l: dec3_q2
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
If your performance is very good, you can earn a <b>BONUS</b> on each game. <br>
<li> Before starting, every task will indicate the bonus amount.
<li> After completing each game, you will be informed about your performance and the threshold for qualifying for the bonus.
</ul><br>
The bonus for this game is <b>$0.05. </b>
- Continue

l: random_game_d31
t: set
- random from 1 2 3

l:
t: jump
- if $random_game_d31 == 1 then goto game1_d31
- if $random_game_d31 == 2 then goto game2_d31
- if $random_game_d31 == 3 then goto game3_d31

l: game1_d31
t: experiment
- d3nbackt

l:
t: jump
- if $random_game_d31 == 1 then goto dec3_q3

l: game2_d31
t: experiment
- d3stroopnp

l:
t: jump
- if $random_game_d31 == 2 then goto dec3_q3

l: game3_d31
t: experiment
- d3iowagt

l:
t: jump
- if $random_game_d31 == 3 then goto dec3_q3

l: dec3_q3
t: radio
q: The available bonus for this Game is <b>$0.04. </b><br>
- Continue

l: random_game_d32
t: set
- if $random_game_d31 == 1 then 2
- if $random_game_d31 == 2 then 3
- if $random_game_d31 == 3 then 1

l:
t: jump
- if $random_game_d32 == 1 then goto game1_d32
- if $random_game_d32 == 2 then goto game2_d32
- if $random_game_d32 == 3 then goto game3_d32

l: game1_d32
t: experiment
- d3nbackt1

l:
t: jump
- if $random_game_d32 == 1 then goto dec3_q4

l: game2_d32
t: experiment
- d3stroopnp1

l:
t: jump
- if $random_game_d32 == 2 then goto dec3_q4

l: game3_d32
t: experiment
- d3iowagt1

l:
t: jump
- if $random_game_d32 == 3 then goto dec3_q4

l: dec3_q4
t: radio
q: The available bonus for this Game is <b>$0.03. </b><br>
- Continue

l: random_game_d33
t: set
- if $random_game_d31 == 1 or $random_game_d32 == 2 then 3
- if $random_game_d31 == 2 or $random_game_d32 == 3 then 1
- if $random_game_d31 == 3 or $random_game_d32 == 1 then 2

l:
t: jump
- if $random_game_d33 == 1 then goto game1_d33
- if $random_game_d33 == 2 then goto game2_d33
- if $random_game_d33 == 3 then goto game3_d33

l: game1_d33
t: experiment
- d3nbackt2

l:
t: jump
- if $random_game_d33 == 1 then goto endquestion

l: game2_d33
t: experiment
- d3stroopnp2

l:
t: jump
- if $random_game_d33 == 2 then goto endquestion

l: game3_d33
t: experiment
- d3iowagt2

l:
t: jump
- if $random_game_d33 == 3 then goto endquestion

## survey dec 3 endsurvey ########################

l: endquestion
t: radio
q: <b><font color='red'>Post-Task</font></b>: This notice concerns <b>your payment.</b><br>
<ul>
<li> You are required to complete post-task questions.
</ul><br>
- Continue

l:
t: jump
- if $random_treat == 11 then goto enjoy
- if $random_treat == 12 then goto enjoy
- if $random_treat == 13 then goto enjoy
- if $random_treat == 14 then goto enjoy

random: begin

l: secon2
t: radio
q: After completing game number one. The bonus in the <b>second game</b> was:
- Lower than
- Equal to
- Higher than

l: subseq2
t: radio
q: After completing game number one. The bonus in the <b>subsequent 2 games</b> were:
- Lower than
- Equal to
- Higher than

random: end

random: begin

l: enjoy
t: radio
q: Did you find the task <b>enjoyable</b>?
- Less than expected
- Appropriate
- More than expected

l: satisfied
t: radio
q: Are you satisfied with your <b>performance</b> on the games?
- Less than expected
- Appropriate
- More than expected

l: difficulty
t: radio
q: Do you feel the <b>difficulty</b> of the task performed was:
- Lower than expected
- Appropriate
- Higher than expected

l: timing
t: radio
q: Do you feel the <b>timing</b> of the task performed was:
- Shorter than expected
- Appropriate
- Larger than expected

l: compajust
t: radio
q: Do you feel the compensation should be <b>adjusted</b> by:
- (-$0.01)
- (-$0.03)
- (-$0.05)
- (+$0.01)
- (+$0.03)
- (+$0.05)

l: compensation
t: radio
q: Do you feel the <b>compensation</b> received for the task performed was:
- Lower than expected
- Appropriate
- Higher than expected

random: end

l: finalcomplete
t: radio
q: <b>You have <b>successfully completed</b> the task</b>. Thank you for your participation! <br>
- Finish
 
## be aware notice and treatment randomization ########################


l: be_aware
t: radio
q: <b><font color='red'>Be aware</font></b>: This notice concerns <b>your payment.</b><br>
Make sure you understand the following:
- Continue

l: random_treat
t: set
- random from 10 11 12 13 14

l:
t: jump
- if $random_treat == 10 then goto nb_q2
- if $random_treat == 11 then goto nb1_q2
- if $random_treat == 12 then goto nb2_q2
- if $random_treat == 13 then goto nb3_q2

## survey abs1 treatment1abs1 ########################

l: nb_q2
t: radio
q: The available budget for this HIT is $0.03 ONLY considers base payment.<br>
- Continue

l:
t: jump
- if $nb_q2 == 1 then goto nbrandom_game_a11

l: nb1_q2
t: radio
q: The available budget for this HIT is $0.09 ONLY considers base payment.<br>
- Continue

l:
t: jump
- if $nb1_q2 == 1 then goto nbrandom_game_a11

l: nb2_q2
t: radio
q: The available budget for this HIT is $0.09 ONLY considers base payment.<br>
- Continue

l:
t: jump
- if $nb2_q2 == 1 then goto nbrandom_game_a11

l: nb3_q2
t: radio
q: The available budget for this HIT is $0.09 ONLY considers base payment.<br>
- Continue

l:
t: jump
- if $nb3_q2 == 1 then goto nbrandom_game_a11

### Rand

l: nbrandom_game_a11
t: set
- random from 1 2 3

l:
t: jump
- if $nbrandom_game_a11 == 1 then goto game1_a11
- if $nbrandom_game_a11 == 2 then goto game2_a11
- if $nbrandom_game_a11 == 3 then goto game3_a11

l: game1_a11
t: experiment
- a1nbackt

l:
t: jump
- if $nbrandom_game_a11 == 1 then goto abs1_q3

l: game2_a11
t: experiment
- a1stroopnp

l:
t: jump
- if $nbrandom_game_a11 == 2 then goto abs1_q3

l: game3_a11
t: experiment
- a1iowagt

l:
t: jump
- if $nbrandom_game_a11 == 3 then goto abs1_q3

l: abs1_q3
t: radio
q: Game 2.<br>
- Continue

l: nbrandom_game_a12
t: set
- if $nbrandom_game_a11 == 1 then 2
- if $nbrandom_game_a11 == 2 then 3
- if $nbrandom_game_a11 == 3 then 1

l:
t: jump
- if $nbrandom_game_a12 == 1 then goto game1_a12
- if $nbrandom_game_a12 == 2 then goto game2_a12
- if $nbrandom_game_a12 == 3 then goto game3_a12

l: game1_a12
t: experiment
- a1nbackt1

l:
t: jump
- if $nbrandom_game_a12 == 1 then goto abs1_q4

l: game2_a12
t: experiment
- a1stroopnp1

l:
t: jump
- if $nbrandom_game_a12 == 2 then goto abs1_q4

l: game3_a12
t: experiment
- a1iowagt1

l:
t: jump
- if $nbrandom_game_a12 == 3 then goto abs1_q4

l: abs1_q4
t: radio
q: Game 3.<br>
- Continue

l: nbrandom_game_a13
t: set
- if $nbrandom_game_a11 == 1 or $nbrandom_game_a12 == 2 then 3
- if $nbrandom_game_a11 == 2 or $nbrandom_game_a12 == 3 then 1
- if $nbrandom_game_a11 == 3 or $nbrandom_game_a12 == 1 then 2

l:
t: jump
- if $nbrandom_game_a13 == 1 then goto game1_a13
- if $nbrandom_game_a13 == 2 then goto game2_a13
- if $nbrandom_game_a13 == 3 then goto game3_a13

l: game1_a13
t: experiment
- a1nbackt2

l:
t: jump
- if $nbrandom_game_a13 == 1 then goto endquestion

l: game2_a13
t: experiment
- a1stroopnp2

l:
t: jump
- if $nbrandom_game_a13 == 2 then goto endquestion

l: game3_a13
t: experiment
- a1iowagt2

l:
t: jump
- if $nbrandom_game_a13 == 3 then goto endquestion

## survey abs1 endsurvey ########################

l: endquestion
t: radio
q: You have completed the task.<br>
- Finish
