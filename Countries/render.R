
library(quarto)
library(tidyverse)


P <- read_csv('/Users/lukefischer/Dropbox/The PopuList Repo/Data/P.csv')


countries <- P |> 
  distinct(country) |>
  mutate(country = case_when(
    country == "Czech_Republic" ~ "Czech Republic", 
    country == "United_Kingdom" ~ "United Kingdom", 
    TRUE ~ country)) |> 
  pull(country) |> 
  as.character()


texts <- c(
# Austria ----
  "
### Parties represented in parliament: \n\n
  
The Freiheitliche Partei Österreichs (Freedom Party of Austria, FPÖ) was founded in 1956, but is
generally considered as a far-right and populist party from 1986 onward, when Jörg Haider became
party leader. Under Haider, who left the party in 2005, the party adopted more xenophobic positions
and intensified its populist rhetoric. In the past decades, the FPÖ has experienced varying levels of
electoral success and several spells in government, in coalition with the conservative ÖVP.
Parties not/no longer represented in parliament: \n\n

The Bündnis Zukunft Österreich (Alliance for the Future of Austria, BZÖ) split off from the FPÖ in
2005. Haider became the first party leader and the BZÖ took over the Freedom Party’s position in
the governing coalition. After Haider's death in 2008, the BZÖ softened its stances on immigration
and toned down its populist rhetoric. The party is therefore no longer coded as far right from 2008
onward. In the 2013 elections the party failed to make it into parliament.\n\n

In 2004, the former leader of the Austrian social democrats in the European Parliament (EP), HansPeter Martin, founded his own party: Liste Hans-Peter Martin (Hans-Peter Martin’s List). It
campaigned based on an anti-corruption, pro-transparency and Eurosceptic platform. While it
generally favoured the principle of European integration, the party focused its criticism explicitly on
the institutional structure of the EU and the national political establishment. As such, the party was
both populist and Eurosceptic. Although Martin’s party gained 2 seats in the EP, it failed to cross the
threshold for the national parliament (with 2.8% of the vote in 2006).\n\n

The Liste Pilz, later named JETZT - Liste Pilz (NOW – Pilz List), is a party with an ambiguous
ideological platform. Founded by prominent former Green Party member Peter Pilz in 2017, the
party gained 4.4% of the vote and 8 seats in the 2017 parliamentary election, which it lost again in
the 2019 election. A highly personalised party, Liste Pilz voiced an eclectic set of demands, including
left-wing anti-austerity policies and criticism of ‘political Islam’ (yet its general stance on sociocultural positions was not far right). Pilz has uttered occasional populist criticisms of economic elites
and politicians. Experts disagree on whether the party should be classified as populist and therefore
we consider it a borderline case.\n\n

In 2013, Team Stronach (TS) entered parliament with 5.7% of the vote. Its founder was the
octogenarian millionaire businessman Frank Stronach, who campaigned on a populist, Eurosceptic,
anti-bureaucracy and anti-tax platform and called for a democratisation of the Austrian political 
system. Unlike its Austrian far right populist counterparts, immigration was not a central issue for
Team Stronach. The party was dissolved in 2017.\n\n
  ",
# Belgium ----
  "
### Parties represented in parliament: \n\n
  
The Nieuw-Vlaamse Alliantie (New Flemish Alliance, N-VA) was founded in 2001 by a right-leaning
faction of the Flemish nationalist Volksunie (People’s Union, VU). The party is in favour of a gradual
secession of Flanders from Belgium. Although the party has elitist roots, it has become increasingly
populist over the years – also embracing, at times, a people-centric discourse. When it comes to
cultural issues, the party has moved closer to the far right. We have therefore classified the N-VA as
both borderline populist and far right.\n\n

The Vlaams Belang (Flemish Interest, VB) was founded as the Vlaams Blok (Flemish Block) in 1978
and is one of the oldest populist and far-right parties in Europe. The initial goal of the party was
Flemish independence, but soon after its foundation the party became increasingly anti-immigrant.
In 2004, the party was dissolved after it was condemned for racism and continued as the Flemish
Interest. Other parties in Belgium have established a cordon sanitaire against the VB – i.e., they
refuse to cooperate with it in any way or at any level of government.\n\n

The Partij van de Arbeid van België/Parti du Travail de Belgique (Workers' Party of Belgium,
PVDA/PTB) is a Marxist party, founded in 1979. The party is the only Belgian party in parliament
(other parties in Belgium are either Flemish or Wallonian). The PVDA/PTB was a rather small party
until the mid-2010s. From then onwards, the party increased its popularity, in particular in Wallonia.
The PVDA/PTB is far left because of its adoption of radical Marxist principles, and also moderately
Eurosceptic. Although the party does incorporate some populist elements in its rhetoric, overall it
does not have a populist conception of politics. As such we have not classified this party as populist
or borderline populist.\n\n

### Parties not/no longer represented in parliament:\n\n

The Front National (National Front, FN), the Walloon namesake of the French Front National, was
founded in 1985 and propagated a strong unitary Belgian nationalism. The party borrowed many
ideas of Jean-Marie Le Pen’s party with the same name in France. Between 1995 and 2007, the party
received about 2% of the vote. After 2010, the party disappeared from the Chamber of
Representatives.In 2012 the party changed its name to Démocratie Nationale (National Democratie,
DN).\n\n

The populist Lijst Dedecker (List Dedecker, LDD) entered the political stage with 4% of the vote in
2007. This was a neoliberal party, highly critical of the established political order and the EU. The
subsequent elections in 2010 led to a big electoral loss of 4 out of 5 seats. The party has not been
represented in parliament since 2014.\n\n

The populist and far-right Parti Populaire (People’s Party, PP), founded by Rudy Aernoudt and
Mischaël Modrikamen, was active between 2009-2019. This party never received more than 1.5% of
the vote, but it did obtain a parliamentary seat in 2010 and 2014. The party does no longer exist.
  ",
# Bulgaria ----
  "
### Parties represented in parliament:
\n\n
Grazhdani za Evropeĭsko Razvitie na Bŭlgariya (Citizens for European Development of Bulgaria,
GERB) is a right-wing populist party founded in 2006 by former National Movement Simeon
II/National Movement for Stability and Progress (NDSV) member and mayor of Sofia Boyko Borisov.
GERB is a conservative and pro-EU force self-professedly based on European Christian Democratic
values. Borisov rose to prominence as the head of a highly personalistic venture, emphasising the
interests of ordinary people (and the ability to solve their problems) and criticising party-political as
well as other elites. The party presents itself as reformist and economically liberal. GERB has been a
central force in Bulgarian politics since the late 2000s and Borisov served three times as prime
minister: between 2009-2013, 2014-2017, and 2017-2021. The party suffered internal splits and a
decline in support after its last stint in government, which was characterised by prolonged protests
against endemic corruption and state capture. In the three elections that followed in 2021, GERB
obtained respectively 26.2%, 23.5% , and 22.4% of the vote amid declining turnout. Borisov’s party
regained some ground and ended up first in the 2022 early election, scoring 24.5% of the vote.
\n\n
Vazrazhdane (Revival) is a populist far-right party founded in 2014 by former Bulgarian National
Movement (VMRO) and National Front for the Salvation of Bulgaria (NFSB) member Kostadin
Kostadinov. The party is fiercely pro-Russian and Eurosceptic with strong anti-Roma, antiimmigration, and anti-LGBTQI+ views. Revival remained at the margins of the Bulgarian electoral
landscape until the November 2021 snap election. , when it crossed the electoral threshold securing
4.9% of the vote. In the 2022 early election, Revival ranked fourth with 9.8% of votes. In this
occasion, the party was largely seen to have capitalised on its pro-Russian stance amid the Russian
invasion of Ukraine and ensuing cost-of-living crisis.
\n\n
### Parties not/no longer represented in parliament:
\n\n
Ataka (Attack) is a populist far-right party founded in 2005 by former TV host Volen Siderov. The
party platform centres on anti-minority, anti-corruption, pro-Russian, and anti-Western views
(hereby including Euroscepticism). Ataka has been one of the most successful far-right parties in the
recent history of Bulgaria. returning 8.1% and 9.4% of the vote, respectively in 2005 and 2009.
Siderov notably made it to the second round of the presidential election in 2006. The party offered
external support to the GERB-led coalition government in 2009 and then acted as kingmaker of the
largely technocratic government led by the Bulgarian Socialist Party and the ethno-liberal Movement
for Rights and Freedoms in 2013. Amid internal scandals and infights, the party’s support has
decreased over time. However, in 2017, Ataka ran as part of the United Patriots (OP) alliance,
including the Bulgarian National Movement (VMRO) and National Front for the Salvation of Bulgaria
(NFSB), which scored 9.1% of the vote and joined the Borisov III government (2017-2021). In 2019,
Ataka bitterly left the far-right alliance and the government altogether. Party support has
plummeted ever since, as also testified by the abysmal results in the four elections held between
2021 and 2022.
\n\n
Izpravi se.BG (Stand Up.BG, ISBG) is a populist anti-establishment and anti-corruption party which
has run as part of the coalition named Stand Up! Mafia, Get Out! (ISMV, until July 2021) and Stand
Up.BG! We Are Coming! (IBG-NI, since July 2021). The coalition included reformist, liberal, and proEU actors. The coalition was established in February 2021. ISBG came to prominence during the antigovernment protests of 2020-2021. While securing representation in the first two elections of 2021,
the coalition failed to pass the threshold in the snap election of November 2021 (2.3% of the vote).
ISBG ran independently in the 2022 early election, but again failed to pass the electoral threshold
(1.0% of the vote).
\n\n
Ima Takav Narod (There Is Such a People, ITN) is a populist party founded in 2020 and led by singer
and cable TV host Slavi Trifonov. The party is anti-establishment, anti-corruption, social conservative,
and pro-EU. ITN returned 17.4% of the vote in the first election contested in April 2021. As the
second most-voted party, it was offered the mandate to form a government, but refused to do so for
the lack of shared principles among elected parties. In the snap election of July 2021, ITN ranked first
scoring 23.8% of the vote, but still failed to form a government coalition after unsuccessful talks with
the liberal Democratic Bulgaria, anti-corruption Stand Up.BG, and the BSP for Bulgaria alliance led by
the Bulgarian Socialist Party. The party paid for its inconsistency and inexperience, and returned only
9.4% of the vote in the snap election of November 2021. After joining the Petkov I government
(2021-2022), Trifonov’s party announced to leave the cabinet over the decision to lift Bulgaria’s veto
on North Macedonia EU accession talks. Such a decision triggered the 2022 early election, in which
ITN scored 3.7% of the vote and failed to re-enter parliament. 
\n\n
The Natsionalno Dvizhenie za Stabilnost i Vazhod (National Movement for Stability and Progress,
NDSV; formerly known as the Natsionalno Dvizhenie \"Simeon Vtori\", National Movement Simeon
II) is a populist party founded in 2001 by Simeon Saxe-Coburg-Gotha (Simeon II), the last tsar of the
Kingdom of Bulgaria between 1943 and 1946. The NDSV adopts an anti-corruption and reformist
platform. was met with enthusiasm upon establishment and the party returned 42.7% of the vote in
the first election contested in 2001. Simeon II then served as PM of a grand coalition comprising the
right-wing Union of Democratic Forces, the left-wing nationalist Bulgarian Socialist Party (BSP), and
the ethno-liberal Movement for Freedom and Rights (DPS). The party largely failed to deliver on its
promises and more than halved its support in 2005 (19.9%), but still took part in the next governing
coalition alongside the BSP and the DPS. The party did not pass the threshold in 2009 and failed to
re-enter parliament since.
\n\n
The Natsionalen Front za Spasenie na Bŭlgariya (National Front for the Salvation of Bulgaria,
NFSB) is a populist far-right party established by Valery Simeonov as a split from the populist farright Ataka in 2011. It is an anti-establishment, anti-immigration, anti-minority, and Eurosceptic
party. The NFSB endorses government spending and is protectionist in terms of Bulgarian business.
The party ran alongside the populist far-right Bulgarian National Movement (VMRO) as part of the
Patriotic Front in 2014, returning 7.3% of the vote. The NFSB and VMRO teamed up with Ataka for
the United Patriots (OP) electoral alliance, which scored 9.1% of the vote and joined the Borisov III
government (2017-2021). With Ataka leaving the OP in 2019, the party contested the 2021 elections
first in alliance with VMRO and the populist far-right Volya. Come the November 2021 snap election,
the NFSB led another far-right coalition, this time without the abovementioned parties. The party
did not manage parliamentary representation in any of the four elections held between 2021 and
2022.
\n\n
Bŭlgariya Bez Tsenzura (Bulgaria Without Censorship, BBT; after 2017 known as Prezaredi
Bŭlgariya, Reload Bulgaria, PB) was a populist and borderline far-right party founded in 2014 by
Nikolay Barekov. The party was anti-corruption, economically liberal, and Eurosceptic, and
presented opposition to Turkey and the Bulgarian Turkish minority as well as anti-immigration as
secondary features of its ideological profile. The party entered parliament in 2014 returning 5.7% of
the vote after a positive performance in the European Parliament election of the same year. In 2017,
the party changed its name to Reload Bulgaria but did not participate in the general election and did
not re-enter parliament since.
\n\n
Red, Zakonnost i Spravedlivost (Order, Law and Justice, RZS) was a populist far-right party founded
in 2005 by Yane Yanev as a rebranding of the National Association – Bulgarian Agrarian People’s
Union. The party was concerned with radical constitutional reform, anti-corruption, and opposition
to Muslim fundamentalism and Islamisation. The party entered parliament in 2009 with 4.7% of the
vote and offered external support to the Borisov I government (2009-2013). RZS was dissolved in
2013.
\n\n
The VMRO – Bŭlgarsko Natsionalno Dvizhenie (Internal Macedonian Revolutionary Organisation –
Bulgarian National Movement, VMRO) is one of the longest-living populist far-right forces of
Bulgaria. It formed as a cultural association in 1991, registered as a party in 1999, and is led by
Krasimir Karakachanov. VMRO claims direct continuity with the historical organisation of the same
name, which was established in 1893. The party is populist far-right, with a strong pan-Bulgarian,
anti-minority, anti-immigration, social conservative, and Eurosceptic agenda. VMRO frequently
contested elections as part of ephemeral electoral coalitions, but in 2014 formed the Patriotic Front
coalition alongside the National Front for the Salvation of Bulgaria (NFSB), returning 7.3% of the
vote. Karakachanov also stood as presidential candidate for the United Patriots (including VMRO,
NFSB, and Ataka) in 2016, ranking third overall. The United Patriots gained 9.1% of the vote in 2017 
and was asked to join the Borisov III government (2017-2021), in which Karakachanov was Minister
of Defence. In the four early elections held between 2021 and 2022, VMRO first ran alone (3.6%,
April 2021), then in coalition with the NFSB and Volya (3.1%, July 2021), and again on its own (1.1%,
November 2021; 0.8%, October 2022), failing to cross the electoral threshold in each occasion.
\n\n
Volya (Will) is a populist far-right party with an anti-corruption, anti-immigration, anti-Western, and
Eurosceptic agenda. The party, led by Veselin Mareshki, operated under different names until 2016
and remained electorally marginal until 2017., when it entered parliament with 4.1% of the vote.
Volya provided support to the Borisov III government (2017-2021), comprising the populist rightwing GERB and the populist far-right United Patriots coalition. In 2021, Volya first contested
elections in alliance with the National Front for the Salvation of Bulgaria and the Bulgarian National
Movement, and then on its own, failing to return any seats in each of the three occasions.
  ",
# Croatia ----
  "
### Parties represented in parliament:
  \n\n
The Domovinski Pokret (Homeland Movement, DP) is a populist far-right party founded in 2020 by
singer, former Christian Democratic Union MP, and presidential candidate Miroslav Škoro. The party
is nationalist, social conservative, economically protectionist and critical of the process of
privatisation, yet fully supportive of private property, anti-corruption, and Eurosceptic. The party led
an alliance with the far-right platform Croatian Sovereigntists (including the Croatian Conservative
Party, Croatian Growth, and the Bloc for Croatia) in the 2020 election, returning 10.9% of the vote.
The Croatian Sovereigntists however left the alliance shortly after over internal disputes. The party
has been marred by defections and Škoro himself left the DP in 2021.
\n\n
The Hrvatska Demokratska Zajednica (Christian Democratic Union, HDZ) is a former populist, farright, and Eurosceptic party founded in 1989 by former president of Croatia Franjo Tuđman. During
Tuđman’s tenure at the helm of the party, the HDZ qualified as nativist, anti-Serbian, anticommunist, social conservative, pro-market, and Eurosceptic. The party was the main driver behind
Croatia’s independence in 1991 and responsible for the process of privatisation in the country after
the dissolution of Yugoslavia. The HDZ underwent a process of moderation after the death of
Tuđman in 1999 and now qualifies as a conservative and pro-EU force.
\n\n
The Hrvatski Demokratski Savez Slavonije i Baranje (Croatian Democratic Alliance of Slavonia and
Baranja, HDSSB) is a populist and borderline far-right party founded in 2006 by convicted war
criminal Branimir Glavaš following his ousting from the Croatian Democratic Union (HDZ). The party
is ethno-regionalist, anti-corruption, promotes the economic advancement of Slavonja and Baranja,
and defines itself as pro-EU. The HDSSB is a relatively marginal force and delivered 1 MP as part of
the coalition led by the HDZ in 2020 (which scored 37.26% of the vote). The party has provided
support to the government led by the HDZ since 2020.
\n\n
Hrvatski Suverenisti (Croatian Sovereigntists, HS) is a populist far-right platform launched in 2019,
which has operated as a fully fledged party since 2021. The platform originally comprised the farright Croatian Conservative Party (HKS), Croatian Growth (Hrast), and the Bloc for Croatia (BzH),
which later merged into the HS. The party is nationalist, social conservative, economically
protectionist, and Eurosceptic. Before formalising its transition into a party, HS members teamed up
with Miroslav Škoro’s Homeland Movement (DP) in the 2020 election, collectively returning 10.9% of
the vote and delivering 4 of the 16 MPs of the electoral alliance. HS terminated cooperation with the
DP shortly after the election.
\n\n
Most (Bridge, formerly known as Bridge of Independent Lists) is a populist and borderline far-right
party founded in 2012 by Božo Petrov. The party brought together members of different civic
initiatives and is anti-establishment, anti-corruption, reformist, economically liberal, fiscally
conservative, and has recently taken social conservative and anti-LGBTQI+ views. Most entered
parliament in 2015 with 13.2% of the vote and supported the short-lived government led by the
Christian Democratic Union. The party returned 7.4% of the vote in the 2020 election and currently
sits in opposition.
\n\n
Možemo! (We Can!) is a populist far-left party founded in 2019. The platform brings together
several left-libertarian, democratic socialist, environmentalist, and anti-corruption movements and
initiatives. Možemo! contested the 2020 election gaining 7% of the vote and attaining parliamentary
representation. The following year, Tomislav Tomašević won the mayorship of Zagreb heading the
Green–Left Coalition.
\n\n
### Parties not/no longer represented in parliament:
\n\n
The Bandić Milan 365 – Stranka Rada i Solidarnosti (Bandić Milan 365 – Labour and Solidarity
Party, BM365) is a populist centrist party founded in 2015 by long-serving Zagreb mayor Milan
Bandić. The party is anti-establishment, committed to social justice and increasing jobs, upholding a
critical outlook on the process of transition and privatisation that took place in the country. BM365
managed 4.0% of the vote in the 2016 election as part of a coalition list also including the People’s
Party – Reformists. BM365 did not enter parliament in 2020. Its leader Bandić died in 2021.
\n\n
The Hrvatska Čista Stranka Prava (Croatian Pure Party of Rights, HČSP) is a marginal far-right party
founded in 1992 and claiming affiliation with the party of the same name established in 1904, itself
split from the historical Party of Rights. The party is ideologically extremist, anti-minority, social
conservative, supporting the legacy of the Ustaše and Ante Pavelić, and anti-NATO and hard
Eurosceptic. The HČSP never managed parliamentary representation and peaked at 2.8% of the vote
in 2011, when it contested elections in alliance with the far-right Croatian Party of Rights Dr. Ante
Starčević.
\n\n
The Hrvatska Laburisti-Stranka Rada (Croatian Labourists-Labour Party, HL-SR) is a populist party
founded in 2010 by Dragutin Lesar. The party is social democratic, progressive, and pro-EU. The HLSR entered parliament in 2011 with 5.2% of the vote and joined the coalition Croatia Is Growing led
by the Social Democratic Party, which returned 33.2% of the vote in the 2015 election. The decision
to join the alliance spurred internal dissent and the party subsequently fell into electoral marginality.
The Hrvatska Stranka Prava (Croatian Party of Rights, HSP) is a far-right party founded in 1990 and
claiming historical affiliation with the Party of Rights established in 1861. The party is ideologically
extremist, anti-minority, anti-establishment, social conservative, environmentalist, and Eurosceptic.
\n\n
The HSP peaked at 7.1% of the vote in the 1992 election but failed to re-enter parliament after 2011.
The Hrvatska Stranka Prava Dr. Ante Starčević (Croatian Party of Rights Dr. Ante Starčević, HSP AS)
was a marginal far-right party founded in 2009 by Ruža Tomašić as a splinter from the far-right
Croatian Party of Rights. The party was ideologically extremist, anti-minority, social conservative,
and Eurosceptic. The HSP AS delivered 1 MP (Tomašić herself) in 2011 as part of a coalition with the
far-right Croatian Pure Party of Rights and 3 MPs in 2015 as part of an alliance with the right-wing
Christian Democratic Union. Tomašić left the party in 2014 and joined the Croatian Conservative
Party shortly after. The HSP AS eventually dissolved in 2020.
\n\n
Živi Zid (Human Shield, ŽZ) is a populist party founded in 2011 and led by Ivan Vilibor Sinčić. The
party is ideologically ambiguous. It has anti-establishment, anti-corruption, anti-eviction, libertarian,
economically protectionist, anti-NATO, and Eurosceptic views, and has taken anti-immigration
positions over time. The party peaked at 6.2% of the vote in 2015 as part of the Only Option
Coalition but failed to cross the threshold in 2020, when it ran as ‘Enough of Robbery’ returning 2.3%
of the vote.
  ",
# Cyprus ----
  "
### Parties represented in parliament:
\n\n
The Anorthotikó Kómma Ergazómenou Laoú (Progressive Party of Working People, AKEL) was
established in 1941 as the continuation of the Communist Party of Cyprus, founded in 1926. AKEL is
one of the two main parties in Cyprus and has served both as government and main opposition. It is
a far left and borderline populist party which describes itself as the vanguard of the working class
guided by Marxist-Leninist principles. Although the party has moderated over time, it continues to
use anti-system language that mobilises on anti-imperialist positions, and aspires towards an
independent and demilitarised Cyprus.
\n\n
The Ethniko Laiko Metopo (National Popular Front, ELAM) was founded in 2008 as a Golden Dawn
(GD) branch in Cyprus. Like GD, ELAM is an extreme ultra-nationalist party whose ideology centres
on an ethnic understanding of the nation, which only includes Greek Cypriots. The party opposes
representative democracy and is openly racist. Scholars debate the compatibility between populism
and (anti-democratic) extreme right ideologies. Similar to GD, however, ELAM claims to be a popular
movement from below, deriving its legitimacy from the Greek Cypriot people. It also shares GD’s 
anti-system narrative, blaming corrupt and incompetent elites who it deems responsible for the
economic crisis in Cyprus. As such we have classified it as a borderline populist party. Despite their
similarities, GD’s profile is overall more extreme in terms of violence, paramilitarism and the
espousal of Nazism employed in its practices. ELAM has progressively moderated and ‘normalised’
its populist rhetoric.
\n\n
### Parties not/no longer represented in parliament
\n\n
Allileggii (Solidarity), was founded by former DISY MEP Eleni Theocharous in 2016. It is a Greek
nationalist party which defines itself as a patriotic movement set against a bi-zonal, bi-communal
federation for Cyprus. Allileggii is a borderline populist party, because its ideology is only moderately
anti-establishment. It is also borderline far right because its nationalist rhetoric is predominantly
oriented towards the Cypriot question and does not meet the nativist criteria of our classification.
Kinima Enomenon Kyprion Kinigon (Movement of Hunters, KEKK) was formally established in 2018.
It is a far right party whose ideology combines nationalism, immigration scepticism and
environmental issues. It is also a populist party which competes on an anti-system platform.
Neoi Orizontes (New Horizons) was formed in 1996 as a Greek nationalist party competing on a farright, populist platform. In 2004 it merged into EVROKO (European Party), also a Greek nationalist
party taking a hard line on the Cyprus issue. In 2016 EVROKO merged into Alilegii.
\n\n
Symmaxia Politon (Citizens’ Alliance, SYN-SYPOL) was founded in 2013 by former AKEL MP Georgios
Lillikas. This was a populist party with nationalist inclinations, which presented itself as a movement
against austerity and old politics. There is debate as to whether this was a far left party. Many party
members did not advocate a far left ideology. The party attracted some of AKEL’s more nationalist
members. As such we have classified it as borderline far left. It was dissolved in 2021 and its
members formally incorporated into the Movement for Social Democracy (EDEK).
  ",
# Czech Republic ----
  "
### Parties represented in parliament:
\n\n
Akce Nespokojených Občanů (Action for Dissatisfied Citizens, ANO) is a populist party founded in
2012 and led by multimillionaire businessman Andrej Babiš. The agenda of the party was chiefly
reformist, but has progressively veered towards economic redistribution. ANO originally had a
pronounced anti-corruption stance, although Babiš was embroiled in cases of fraud and misuse of
EU funds in relation to his own Agrofert company, thus tarnishing its credentials. The personalistic
nature of the party has been an impetus to infighting and departures over the years. ANO has
occasionally also delivered Eurosceptic remarks and, while in government, voiced its opposition to
the EU migrant relocation and resettlement scheme. ANO is generally liberal with regard to
immigration, but the party has selectively upheld an anti-refugee stance in recent electoral
campaigns. In the first election contested in 2013, the party gained 18.7% of the vote. ANO teamed
up with the Christian-Democrats (KDU-ČSL) to form a government led by the Czech Social
Democratic Party (ČSSD). The party improved its performance in 2017, when it ranked first with
29.6% of the vote. During the 2017-2021 term, Babiš first served as PM of a minority government
(2017-2018) and then as head of minority coalition comprising the ČSSD, and relying on the external
support of the Communist Party of Bohemia and Moravia (KSČM) and the far-right Freedom and
Direct Democracy (SPD). Between 2018 and 2019, Babiš and his government were targets of large
grassroots mobilisations following a series of political and financial scandals. This did not prevent the
party from making further gains in 2021 (37.1% of the vote), but the party did not make it into
government after the elections.
\n\n
Svoboda a Přímá Demokracie (Freedom and Direct Democracy, SPD) is a populist far-right party
founded in 2015 by Tomio Okamura and split from Dawn of Direct Democracy. The party is antiimmigration, originally economically liberal but currently favouring redistribution, pro-direct
democracy, and hard Eurosceptic. Amid the EU asylum policy crisis, SPD firmly stood in opposition to
resettlement quotas. The party returned 10.6% and 9.6% of the vote, in 2017 and 2021 respectively.
\n\n
### Parties not/no longer represented in parliament:
\n\n
The Komunistická Strana Československa (Communist Party of Czechoslovakia, KSČ) was a far-left
party founded in 1921 and the ruling organisation of the Czechoslovak Socialist Republic between
1948 and 1989. The KSČ reorganised as a federation of the Communist Party of Bohemia and
Moravia and the Communist Party of Slovakia in 1990. The federation managed 13.6% of the vote in
the first democratic elections of 1990 before its eventual dissolution in 1992.
\n\n
The Komunistická Strana Čech a Moravy (Communist Party of Bohemia and Moravia, KSČM) is a
far-left party officially founded in 1990 as a constituent part of the Communist Party of
Czechoslovakia (KSČ) and, after its dissolution, its sole Czech successor. The party is statist and anticapitalist, progressive on social and civil rights, Eurosceptic, anti-NATO, pro-Russia and pro-China,
and also programmatically opposed to illegal immigration. The party has managed parliamentary
representation throughout the history of independent Czechia, often listing as the third political
force of the country, yet always sitting in opposition. The KSČM then offered support to the Babiš II
government (2018-2021), bearing significant influence during this period. In 2021, the KSČM failed
to cross the electoral threshold returning 3.6% of the vote.
\n\n
Přísaha (Oath) is a populist party founded in 2021 by Robert Šlachta, former director of an anticorruption police unit. Šlachta ran on a populist, anti-corruption, anti-establishment platform in the
2021 campaign. The party supports law and order, low taxation, a lean state, and transparency. It
called for an audit of all COVID-related procurement by the government. In the 2021 election,
Přísaha nearly missed the threshold, returning 4.7% of the vote. 
\n\n
The Sdružení pro Republiku – Republikánská Strana Československa (Rally for the Republic –
Republican Party of Czechoslovakia, SPR-RSČ; also known as the Republicans) is a far-right party
founded in 1989 by Miroslav Sládek. The party is anti-minority and Eurosceptic. The Republicans list
among the oldest far-right parties of Central and Eastern Europe. After entering parliament in 1992
(6.0% of the vote) and reaching 8.0% of the vote in 1996, the party failed to re-enter parliament
again. The Republicans served as the personal vehicle of Sládek (and his lavish lifestyle) and
underwent several name and organisational changes following, among others, a court ruling
declaring the party bankrupt in 2001. The current incarnation of the Republicans resurfaced in 2016.
The Svobodní (Freedomites, formerly known as the Party of Free Citizens, SSO) is a populist rightwing libertarian, economically liberal, and Eurosceptic party founded in 2009. The party lies at the
margins of the Czech party system, delivering its best performance in 2021 (2.8% of the vote), when
it ran as part of the Trikolóra–Svobodní–Soukromníci coalition.
\n\n
Úsvit (Dawn, formerly known as Dawn of Direct Democracy) was a populist and far-right party
founded in 2013 by Tomio Okamura. The party stood for anti-corruption and the use of directdemocratic means, and openly opposed the EU. During its campaigning, Okamura would also use an
anti-immigration and anti-minority rhetoric. Úsvit returned 6.9% of the vote in the 2013 election.
The party was destabilised by internal rifts and the misuse of state funding by Okamura, and the
leader left the party to form Freedom and Direct Democracy in 2015. Úsvit eventually dissolved in
2018.
\n\n
The Volný Blok (Free Bloc, VB; formerly known as Czech Sovereignty) is a marginal populist and farright party founded in 2011 by Jana Bobošíková and currently led by Jana Volfová. The party is antiimmigration, anti-Islam, anti-vax, anti-LGBTQI+, anti-NATO, and hard-Eurosceptic. In its first
incarnation as Czech Sovereignty, the party scored 3.7% of the vote (2010). The VB returned 1.3% of
the vote in the 2021 election.
\n\n
Věci Veřejné (Public Affairs, VV) was a populist right-wing party founded in 2001. The party was
social conservative, soft authoritarian, pro-EU, and advocated anti-corruption and direct democracy.
In 2010, VV managed 10.9% of the vote and entered a government coalition with the Civic
Democratic Party and TOP 09. The journalist Radek John, VV leader between 2009 and 2013, served
as Minister of Interior (2010-2011); Vít Bárta, one of VV’s leading figures, as Minister of Transport
(2010-2011). The government coalition was however short-lived, undermined by a corruption
scandal involving the same Bárta, who was subsequently expelled from the party. VV was beset by
defections and did not field candidates in the 2013 election, and eventually dissolved in 2015.
  ",
# Denmark ----
  "
### Parties represented in parliament:
\n\n

The Dansk Folkeparti (Danish People's Party, DF) was founded in 1995 by Pia Kjærsgaard. The party
is a populist and far-right party, although it is slightly more moderate than many other parties
belonging to this party family. This is probably due to the fact that the party has supported a liberalconservative government coalition for many years, and, hence, had to compromise. The party has
influenced Denmark’s immigration policy. By keeping one foot in and one foot out of government,
the DF managed to keep its outsider profile. During the 2019 national elections, the party lost a large
share of its supporters.
\n\n

Another far-left party in Denmark is Enhedslisten – De Rød-Grønne (The Red–Green Alliance, En-O),
which was founded in 1989 as an alliance of three left-wing parties (including the communists). The
party positions itself as anti-capitalist and supports green politics and multiculturalism. Although EnO retains a relatively strong revolutionary image and is fairly Eurosceptic, it has significant
experience supporting government coalitions. En-O remains more radical than SF, in particular when
it comes to socioeconomic themes.
\n\n

Another Danish far-right party is Nye Borgerlige (The New Right, NB). The party was founded by two
former members of the Conservatives in 2015, and presents itself as more radical than the DF, in
particular when it comes to immigration. In terms of socio-economic issues, the party distinguishes
itself from the DF by taking more right-wing socio-economic positions. The party has roots in the
Conservative Party, which is a classical mainstream party, and does not have a very outspoken
populist discourse. We therefore classify the party as borderline populist. The party made it into
parliament for the first time in 2019 with 2.4% of the votes.
\n\n

In 1959, the Socialistisk Folkeparti (Socialist People's Party, SF) was founded by a former leader of
the communist party. After embracing new left issues like environmentalism and feminism, the party
managed to expand its voter base beyond the working class. Over time, the SF became one of the
greenest parties on the far left. By now the party is only borderline far left. Similarly, it evolved from
a strongly Eurosceptic party to a party that is relatively positive about European integration. The SF 
has not been Eurosceptic since 2004, when a majority of its members approved the EU draft
constitution. In 2022, it was decided that the English name of the party was \"Green Left\".
\n\n
### Parties not/no longer represented in parliament:
\n\n
In 1972, a charismatic tax lawyer, Mogens Glistrup, founded the populist and far right
Fremskridtspartiet (Progress Party, FrP). Initially, the FrP was an anti-tax and anti-bureaucracy
party. But in the 1980s, the party became increasingly anti-immigrant. In 1995, several FrP politicians
left the party to found the DF (among them both the party leaders Pia Kjærsgaard and Kristian
Thulesen Dahl). The FrP became less and less successful and disappeared from the Folketing in 2001.
  ",
# Estonia ----
  "
### Parties represented in parliament:
\n\n
The Eesti Konservatiivne Rahvaerakond (Conservative People’s Party of Estonia, EKRE) is a populist
far-right party founded in 2012 through the merger of the agrarian People’s Union of Estonia party
and the nationalist Estonian Patriotic Movement. The party largely revolves around Mart Helme
(chairman between 2013 and 2020) and his son Martin Helme (chairman since 2020). EKRE has a
pronounced anti-immigration, anti-Russian, social conservative, and Eurosceptic profile. We
therefore categorise it as Eurosceptic, far right, and populist. The party enjoyed growing popularity
since 2015, when it returned 8.1% of the vote in the general election. EKRE gained 17.8% of the vote
in the 2019 election and was asked to join the government coalition led by the Centre Party (EK).
EKRE remained in government until January 2021, when PM Jüri Ratas (EK) and the whole
government resigned after the breakout of a corruption scandal.
\n\n
The Eesti Keskerakond (Estonian Centre Party, EK) is a centrist party founded in 1991 from the
ashes of the Popular Front of Estonia. The party is a pro-market force programmatically committed
to strengthening the Estonian middle class and social justice. The party has been internally divided
over European issues until the country’s accession to the EU, but is now fully committed to Estonian
membership. The party is often labelled as populist. While presenting a strong people-centric
component – EK advocates greater citizen participation through democratic means – there is no
anti-elitism. Therefore, the EK at best qualifies as borderline populist. The party has often enjoyed
government responsibilities as part of coalitions. The EK returned 23.0% of the vote in the 2019
election and has continuously sat in government between 2016 and 2022.
\n\n
### Parties not/no longer represented in parliament:
\n\n
Eesti Kodanik (Estonian Citizen, EKo) was a populist far-right and Eurosceptic party active between
1992 and 1995. EKo managed 6.9% of the vote in the 1992 election and 3.6% of the vote in the 1995
election, when it ran in alliance with the Better Estonia party.
\n\n
The Res Publica Party (ERP) was a populist right-wing party founded in 2001. The party presented an
anti-establishment agenda campaigning on anti-corruption and ‘law and order’. ERP managed 24.6%
of the vote in the 2003 election and formed a government coalition with the Reform Party and the 
People’s Union. The party merged in 2005 with the Pro Patria Union into the ‘Pro Patria and Res
Publica Union’, which was later renamed Isamaa (Fatherland).
\n\n
The Erakond Res Publica (Estonian National Independence Party, ERSP) was a far-right party
founded in 1988 by groups of nationalist and anti-communist dissidents. The main goal of the party
was to establish a non-communist Estonia (before independence) and – consistent with its antiRussian outlook – making the country the exclusive home for ethnic Estonians (after independence).
The party gained 8.8% of votes in the first free elections of 1992 and joined the right-wing coalition
government between 1992 and 1995. The ERSP merged with the Pro Patria National Coalition to
form the conservative Pro Patria Union (Isamaaliit).
  ",
# Finland ----
  "
### Parties represented in parliament
\n\n
Liike Nyt (Movement Now, Liik) was founded in 2018 by Hjallis Harkimo, politician and celebrity
businessman, together with several other people. The party obtained one seat in parliament, which
was taken up by Hjallis Harkimo. The party is in favour of participatory democracy, attacks elites for
being out of touch with the people, and criticises the EU. Although the party is neither far left nor far
right, it can be classified as populist.
\n\n
Perussuomalaiset (Finns Party [formerly known as the True Finns], PS) was founded in 1995 by
Timo Soini and several others as a successor to the populist Finnish Rural Party. The party slowly
became more popular with voters, peaking at 19.1% of the vote in 2011. In 2015 PS joined a
government coalition with the Center Party and the National Coalition party. Up till this moment, the
PS could be classified as populist and borderline far right due to its relatively moderate antiimmigration stances compared to many other far right parties. However, when the more outspoken
Jussi Halla-aho became the new party leader in 2017, the prime minister declared that he could not
continue a coalition with a party with Halla-aho as its leader. As a result, 20 MPs (including Soini)
split off and formed a new party (Blue Reform, SIN). Given these developments, 2017 is the year in
which our categorization of the party changes from borderline far right to far right.
\n\n
Vasemmistoliitto (Left Alliance, VAS) was founded in 1990 as a successor to the main communist
party in Finland. Already from the outset, it held a particular position within the radical left party
family as it was and still is relatively green and progressive, and has a lot of government experience.
The party presented itself as a ‘third left’ party, focusing on issues like environmentalism and
feminism. As a result, VAS could be conceived of as one of the most moderate left-wing parties
outside the mainstream left in Europe. We therefore classify the party as borderline far left. The VAS
has a relatively stable voter base of slightly less than 10% of the electorate. 
  ",
# France ----
  "
### Parties represented in parliament:
\n\n
In 1999 Nicolas Dupont-Aignan founded Debout La République (Republic Arise, DLR), which later
changed into Debout La France (France Arise, DLF). At first, the party was a souverainist faction in
the Gaullist Rassemblement pour la République (Rally for the Republic, RPR), founded by Jacques
Chirac, but in 2008 it became an autonomous party. DRL/DLF is Gaullist but, since 2017 it has
increasingly adopted nativist stances and therefore it can be classified as borderline far right, though
it is still less populist and nativist than the RN. In socio-economic terms the party is more right-wing.
It is also a Eurosceptic party.
\n\n
Jean-Marie Le Pen founded the Front National (National Front, FN) – since 2018 Rassemblement
National (National Rally, RN) – in 1972 as an amalgam of various far-right groups. The party became
increasingly successful in the 1980s and 1990s. In 2011, Le Pen’s daughter Marine Le Pen took over
the party’s leadership and started a process of so-called dédiabolisation (de-demonisation),
moderated the party’s discourse, suspended and later expelled her more radical father, and changed
the party’s name. Yet in terms of its core ideology and its policy positions, the party remained as
radical as ever. Over time, the FN/RN has modified its positions on the economy, from an initially
economically liberal (anti-tax) party to welfare chauvinist stances. The RN obtained its best electoral
score at the Presidential (23.5%) and legislative elections in 2022, electing 89 MPs. Today, the RN is
relatively left-wing in socio-economic terms, at least in so far as a chauvinist understating of welfare
provisions and state intervention in the economy can be considered left-wing. This economic profile
became evident in the aftermath of the Great Recession and, more recently, during the COVID19
pandemic. RN can be classified as far right, populist and Eurosceptic.
\n\n
La France Insoumise (Unsubmissive France, FI) was founded by Jean- Luc Mélenchon in 2016 from a
split in the Parti de Gauche (Left Party, PG), a former alliance with the Communists and other small
parties of the far left. In the 2022 legislative elections the party was part of the NUPES coalition
(New ecological and social people’s union) together with the Socialist Party (PS), the French
Communist Party (PCF see below), Europe Ecology – The Greens (EELV), Ensemble! (E!), and
Génération.s (G.s). The FI participated in the 2017 presidential elections polling around 20% of the
vote. In the 2022 presidential elections, the FI obtained its best result (21.9% of the vote in the first
round). The FI’s worldview and policy positions are rooted in a far-left agenda. The party has been
populist throughout its existence but it has not emphasised it as much during the 2017 and 2022
campaigns. In the latter campaign, Mélenchon invested more energy in elaborating specific policy
issues, including the economy, welfare, environmental protection, and, following the Ukrainian war,
foreign policy. In 2022, the FI kept pursuing a Eurosceptic agenda, calling for radical changes to EU
treaties.
\n\n
The Parti Communiste Français (French Communist Party, PCF) is one of the oldest communist
parties in Europe. It was founded in 1920, and it was the main French party on the left between
1945-1960 but it became less successful from the 1980s onwards. The PCF participated in a
government coalition between 1997 and 2002. Later, it became a leading member of an electoral
alliance that was created for the 2009 EP elections: the Front de Gauche (Left Front, FdG). The other
main member (alongside other smaller organizations) of this alliance was the Parti de Gauche (Left
Party, PG), a party that was founded in 2009 by Jean-Luc Mélenchon, a former social democrat, but
never contested elections independently. PG positioned itself ideologically between the social
democrats and the PCF. In 2016 Mélenchon abandoned the PG to create the FI (see above). For the
2022 presidential elections, the PCF did not negotiate any alliance and decided to run alone. For the
2022 legislative elections the PCF joined the NUPES coalition (New ecological and social people’s
union) together with he Socialist Party (PS), the French Communist Party (PCF see below), Europe
Ecology – The Greens (EELV), Ensemble! (E!), and Génération.s (G.s). The PCF was and is a traditional
communist party and can therefore be classified as far-left.
  ",
# Germany ----
  "
### Parties represented in parliament
\n\n
The Alternative für Deutschland (Alternative for Germany, AfD) was founded in 2013 as a mostly
Eurosceptic party. Initially, the party was dominated by right-wing academics addressing primarily
socio-economic issues (hence sometimes called the “professors party”). The party has always been
populist. Since its foundation, there has been a struggle between moderate and radical factions. The
leader of the moderates, Bernd Lucke, quit in 2015. Subsequently, the AfD swiftly moved into a far
right direction under Frauke Petry’s leadership with a greater focus on immigration. We classify the
party as far right since 2015, when Petry was elected leader. There is even a group in the party – led
by Björn Höcke – that currently flirts with extremism.
\n\n
After the fall of the Berlin wall, the Partei des Demokratischen Sozialismus (Party of Democratic
Socialism, PDS) was founded as the successor to the Sozialistische Einheitspartei Deutschlands
(Socialist
Unity Party, SED), also known as the (previously ruling) East German Communist Party. The PDS was
active between 1989 and 2007 and was mainly popular among voters in the east of Germany. In
2007 the party merged with a small radical left party from western Germany into Die Linke (The
Left). Although the party has often been classified as a populist radical left party, some scholars are
hesitant about this label. We therefore classify the party as borderline populist, borderline far left
since its foundation as Die Linke in 2007.
\n\n
### Parties not/no longer represented in parliament
\n\n
Die Republikaner (The Republicans, Rep) was founded in the 1980s by former CSU (Bavarian
Christian democrats) politicians and still exists. The party has, however, never gained seats in the
national German parliament. It can be classified as populist and Eurosceptic.
  ",
# Greece ----
  "
### Parties represented in parliament
\n\n
Elliniki Lisi (Greek Solution, EL) is a far-right party founded by former LAOS MPs and ND MP Kyriakos
Velopoulos. The party is populist, far right, and Eurosceptic. It puts forward a nationalist exclusionary
agenda that seeks to “emancipate” Greece and it wants to put ‘Greeks first’ in all policy areas. EL 
emphasises Greek sovereignty and independence and identifies the maintenance of religion (Greek
Orthodoxy) and Greek identity as its main priorities. The party won 3.7% of the popular vote in the
2019 national elections, winning 10 seats.
\n\n
The Kommounistikó Kómma Elládas (Communist Party of Greece, KKE) is a far-left party founded in
1918 as the Socialist Working Party of Greece (SEKE). KKE clearly falls under the extreme left
category. It is communist and hard Eurosceptic. It defines itself as the party of the working class led
by the principles of Marxism-Leninism and proletariat internationalism. KKE aligns its organisational
structures, aims and functions to the 1871 Paris Commune and 1917 Russian Revolution. KKE was
outlawed in the aftermath of the Greek civil war and the communist persecutions. It was legalised
following the restoration of democracy in 1974. Since then, the party has enjoyed consistent
representation in the Greek parliament ranging from 3rd to 5th largest party.
\n\n
Μetopo Evropaikis Realistikis Anipakois (European Realistic Disobedience Front, MeRa25) is a farleft Greek political party founded in 2018, by former SYRIZA Finance minister Yanis Varoufakis,
within the context of the Greek economic crisis. MeRa25 is the Greek branch of the Democracy in
Europe Movement – DiEM25. The party puts forward a populist, Eurosceptic, green and reactionary
left-wing agenda, presenting itself as a front of opposition against economic policies which it claims
have ‘enslaved’ Greece and plunged it into debt. Although the party describes its agenda as
internationalist, it also claims to espouse ‘authentic patriotism’. MeRa25 received 3.44% of the votes
cast during the 2019 national elections, gaining 19 seats in the Greek parliament.
\n\n
The Synaspismós Rizospastikís Aristerás (Coalition of the Radical Left, SYRIZA) was formed in 2004
as an alliance between SYNASPISMOS and 11 left-wing factions. While previously marginalised
within the Greek party system, SYRIZA benefited from the Greek economic crisis and the consequent
environment of political instability. In June 2012, SYRIZA quadrupled its 2009 results and became the
second biggest party in Greece. In 2015, the party managed to attract 36.3% of the Greek vote,
which translated to 149 seats, and formed a government with the far-right ANEL. SYRIZA lost the
2019 elections. Overall, SYRIZA has pursued a far-left, populist and Eurosceptic agenda,
characterised by radicalism and an anti-establishment rhetoric. While in government (2015-2019),
the party’s anti-EU rhetoric, which reached a peak during the July 2015 referendum, waned in favour
of Realpolitik and an attempt to appease Greece’s lenders. Therefore, this party is no longer
classified as far-left post-2015.
\n\n
### Parties not/no longer represented in parliament:
\n\n
Anexartitoi Ellines (Independent Greeks, ANEL) was established in 2012 as a splinter party from the
centre-right New Democracy. The party has a far-right and populist agenda. ANEL formed a surprise
coalition with the left-wing SYRIZA in 2015. While seemingly unlikely bedfellows, the two parties
were united by their anti-bailout stance, converging on their opposition to austerity and external
interference in domestic economic affairs.
\n\n
Dimokratiko Koinoniko Kinima (Democratic Social Movement, DIKKI) was founded in 1995 by
former PASOK Minister Dimitris Tsovolas. The party followed a far-left, populist, Eurosceptic agenda
and opposed the EU’s ‘exploitative neo-liberal policies’. Tsovolas resigned from the party’s
leadership in 2004, after DIKKI failed to meet the 3% threshold for entering parliament. In 2007,
DIKKI was incorporated into SYRIZA. In the summer of 2015, DIKKI left SYRIZA, arguing it had failed to
pursue an EU exit after the outcome of the bailout Referendum, and joined the Popular Unity (Laiki
Enotita), another far left, Eurosceptic party formed as a splinter from SYRIZA. 
  ",
# Hungary ----
  "
### Parties represented in parliament:
\n\n
Fidesz (Alliance of Young Democrats) is a populist far-right and Eurosceptic party, founded in 1988.
The party started out as a liberal progressive force mobilising against the communist regime, but it
underwent significant ideological change, reinventing itself as a conservative force. The ideological
evolution continued through the Orbán I government (1998-2002), when the party started flirting
with nativist themes in an attempt to woo the voters of the far-right Hungarian Justice and Life Party
and its coalition partners, the populist agrarian Independent Smallholders’ Party (FKgP) and the
right-wing Hungarian Democratic Forum (MDF). Despite increasing its electoral performance in 2002
(41.1% of the vote), Fidesz narrowly ranked second. During the period in office, the party decidedly
veered to the far right and Euroscepticism, adopting anti-immigration and anti-LGBTQI+ views, and
setting the country on an autocratic path. Fidesz returned 54.1% of the vote and another two-thirds
majority in the 2022 election. The party has contested elections as part of the Fidesz-KDNP alliance
since 2005. The Christian Democratic People’s Party (KDNP) is a de facto satellite of Fidesz and it
should be considered as part of Fidesz rather than a separate entity.
\n\n
The Jobbik Magyarországért Mozgalom (Movement for a Better Hungary, Jobbik) is a populist
right-wing party founded in 2003 and stemming from a movement of radically conservative Christian
students. The party started out as far right with anti-establishment, anti-minority, irredentist, social
conservative, and Eurosceptic views, and developed a harsh anti-immigration agenda at the peak of
the EU immigration policy crisis of 2015. Jobbik entered parliament in 2010 (16.7% of the vote),
improved its performance in 2014 (20.2%), and consolidated its standing as the main opposition
party in 2018 (19.1%). Jobbik underwent a process of moderation since the mid 2010s.While the
party has notably given up on its Euroscepticism and the most contentious aspects of its far-right
repertoire, it is still committed to nationalist values. Jobbik now self-professes a ‘centrist people’s
party’ profile. As of 2020, Jobbik is part of the United for Hungary political alliance alongside all other 
liberal opposition parties in an attempt to defeat PM Viktor Orbán. The alliance returned 34.5% of
the vote in the 2022 election.
\n\n
Mi Hazánk Mozgalom (Our Homeland Movement, MHM) is a far-right party founded in 2018 by
extremist Jobbik dissidents concerned over the party’s moderation strategy. MHM is ideologically
anti-immigration, anti-minority, irredentist, anti-LGBTQI+, environmentalist, and hard Eurosceptic.
MHM has been represented in parliament since 2018 through a split of Jobbik’s parliamentary
group, played a vocal role in anti-lockdown protests during COVID-19 restrictions in Hungary, and
entered parliament through popular mandate in 2022, with 5.9% of the vote.
\n\n
### Parties not/no longer represented in parliament:
\n\n
The Független Kisgazdapárt (Independent Smallholders’ Party, FKgP) was a populist right-wing
party founded in 1930 and revived in 1988. The party was agrarian, nationalist, conservative, and
anti-communist. The party took part in the first post-communist democratic government between
1990 and 1992 and joined the Fidesz-led Orbán I coalition government in 1998, after gaining 13.8%
of the vote. The FKgP lost parliamentary representation in 2002. The party underwent several
leadership changes and lately took a far-right turn, teaming up with Our Homeland Movement
(MHM) and the moribund Hungarian Justice and Life Party. The party was liquidated in 2021 and its
members joined MHM ranks.
\n\n
The Magyar Igazság és Élet Pártja (Hungarian Justice and Life Party, MIÉP) was a populist far-right
party founded in 1993 by former Hungarian Democratic Forum MP, István Csurka. The party was
anti-minority, anti-Semitic, irredentist, and Eurosceptic. MIÉP managed 5.5% of the vote in the 1998
election, but failed to re-enter parliament in 2002 mostly due to increased turnout. The party
teamed up with the far-right Jobbik in 2006, failing to cross the electoral threshold. MIÉP lied at the
margins of Hungarian politics and eventually merged into the far-right Our Homeland Movement in
2021.
\n\n
The Magyar Munkáspárt (Hungarian Workers’ Party, MMP) is a far-left party founded in 1989 as
the (non-reformist) successor of the Hungarian Socialist Workers' Party, the ruling party of Hungary
between 1956 and 1989. The party is communist, left-wing nationalist, anti-NATO, and Eurosceptic.
The party peaked at 4.1% of the vote in the 1998 election, but always failed to gain parliamentary
presentation.
  ",
# Iceland ----
  "
### Parties represented in parliament
\n\n
Flokkur fólksins (People’s Party, FIF) was founded by Inga Sæland in 2016. It has grown steadily over
the years, receiving 6.9% of the vote in 2017 and 8.9% of the vote in 2021. The party holds a leftwing position on many socio-economic welfare issues and a more right-wing position on cultural
issues. The party employs a populist discourse in its battle against corruption and lawlessness.
The Framsóknarflokkurinn (Progressive Party, FSF) was founded in 1916, representing Icelandic
farmers and people from rural areas. It became one of the biggest parties and participated in several
government coalitions. It obtained its biggest electoral success in 2013, when it gained almost a
quarter of the vote. During the financial crisis, the party changed its course after years of declining
support, taking up an anti-establishment discourse. Therefore, the party is classified as borderline
populist from 2009 onwards. Since a leadership change in 2016, the party has mainstreamed. Hence,
it is no longer classified as populist from 2016 onwards.
\n\n
Miðflokkurinn (Centre Party, M) was founded in 2017 by Gunnlaugsson after his departure from
FSF. M obtained 10.9% in the 2017 elections and 5.5% in 2021 elections. Although the party presents
itself as rather conservative and nationalist it cannot be classified as far right. Given Gunnlaugsson’s
anti-elitist people-centrist discourse, the party is classified as populist.
\n\n
### Parties not/no longer represented in parliament
\n\n
Borgarahreyfingin (Civic Movement, B-H) was founded in response to the financial crisis of 2007-
2008. The party obtained 7.2% of the votes in the 2009 elections, but disappeared from parliament
after that. B-H criticised the political elite and was in favour of radical changes in response to the
recession. It has been classified as populist.
\n\n
The Sósíalistaflokkur Íslands (Icelandic Socialist Party, SFI) was founded in 2017 by editor and
author Gunnar Smári. It received 4.1% at the 2021 elections, but gained no seats due to the electoral
threshold. The party supports the poor and attacks “the rich and those who serve their interests”.
The SFI is categorised as populist and far left.
  ",
# Ireland ----
  "
### Parties represented in parliament
\n\n
Founded in 1905, Sinn Féin (We Ourselves, SF) has strived for a united, independent Irish state and
was historically associated with the paramilitary Irish Republican Army (IRA). In modern times, the
party is both active in the Republic of Ireland and the United Kingdom territory of Northern Ireland.
It plays a different role across political institutions. In the UK House of Commons, it has historically
refused to take up gained seats, while it has been part of the devolved consociationalist executive of
Northern Ireland. In the Republic, Sinn Féin has remained in opposition, but it has witnessed a strong
growth in the most recent parliamentary elections, winning a quarter of the vote in 2020. The Irish
branch of the party can be defined as socio-economically left-wing and populist (though applying the
‘far left’ label would be controversial). Its discourse has been characterised by strong criticism of
mainstream parties and a promise to represent ordinary Irish people in deprived rural and urban
areas. Of the main Irish parties, it has also been the most Eurosceptic. SF’s strong Irish nationalism
has not coincided with nativism, as it has promoted an ‘inclusive’ vision of Irish identity which is not
at odds with multiculturalism.
\n\n
A minor far-left party is the Socialist Party (SP), founded by former Labour Party members. The party
has enjoyed parliamentary representation for most of the years since 1997, with between one and
three representatives. In 2016 and 2020, these ran as candidates for the Anti-Austerity Alliance or
People Before Profit/Solidarity (PBPS) platforms. Given its emphasis on class divisions, the party can
better be seen as an exponent of traditional socialism rather than populism.
\n\n
### Parties not/no longer represented in parliament
\n\n
In 1970, Sinn Féin witnessed a significant split, leading to the creation of, as it is now called, the
Workers' Party (WP). The WP’s agenda was less populist but more radical regarding both its
economic policies, and its militancy in terms of Irish unification. For these reasons it was considered
to be a far-left party. It was last represented in parliament in 1989. A more moderate (non-far left)
split from WP was the Democratic Left (DL), which was founded in 1992 and dissolved in 1999.
Besides political parties, populism can be found among independent politicians in Ireland which
constitute a significant presence in the parliament– yet these are beyond the scope of our database. 
  ",
# Italy ----
  "
### Parties represented in parliament
\n\n
De Luca Sindaco d’Italia (De Luca Mayor of Italy, DLSI) was founded in 2022 by Cateno de Luca and
gained popularity during the first wave of COVID19 containment measures in Italy. Despite the poor
electoral results in the 2022 general elections, the list appointed two MPs. DLSI is considered
populist, because it voices a flamboyant anti-establishment rhetoric. Its positions on EU integration
remain unclear.
\n\n
In 1993, Silvio Berlusconi, a media tycoon, founded Forza Italia (Let's Go Italy, FI) as a response to
the collapse of the party system following a significant corruption scandal (tangentopoli). The party
is highly personalized, with both its structure and ideology revolving entirely around its leader,
deceased in 2023. FI enjoyed great success throughout the 1990s and 2000s, with Berlusconi serving
as Italy's prime minister on multiple occasions. In 2007, Berlusconi established Il Popolo della Libertà
(The People of Freedom, PDL), which emerged from the merger of FI (previously known as Casa
delle Libertà (House of Freedoms, CdL)) and Alleanza Nazionale (National Alliance). The PDL was
dissolved in 2013 and is considered borderline populist throughout its existence. Since 2011, when
the Monti government was established, FI has displayed a borderline Eurosceptic profile, with
Berlusconi openly criticizing the EU but not supporting Italexit. However, when the Lega-M5S
government took office in 2018-2019, Berlusconi began to moderate the Eurosceptic stances, trying
to set FI apart from populist parties.
\n\n
Fratelli d’Italia (Brothers of Italy, FdI) was founded in 2012 and it is headed by Georgia Meloni. It
originated from a fringe of PDL’s MPs contesting the Monti technocratic government that governed
Italy during the Great Recession. The symbol of the party is the tricolour flame, which was also the
symbol of the extinct MSI and AN. In the 2013, 2018 and 2022 general elections, the party ran as
part of a centre-right coalition, together with the League and Forza Italia. At the outset, FdI
concentrated on socio-economic and civil rights issues, with no significant traces of populism in its
campaigns. However, starting from 2014, the party began exhibiting more apparent traits of
populism, nativism, and Euroscepticism in its platform and it now undoubtedly qualifies as a far-right
and populist party. Despite this evolution, FdI still upholds its dedication to social conservatism,
family policies, and the advocacy of small and medium-sized enterprises. This preserves its unique
profile, not just within the right-wing coalition, but also when contrasted with the League.
\n\n
The Lega Nord/Lega (Northern League/League, LN/L) was founded in 1991 by Umberto Bossi as a
federation of several regional parties and movements, including the Liga Veneta. Bossi established
the LN as a secessionist party, in favour of autonomy for the north of Italy. After a major corruption
scandal in 2011, Roberto Maroni became the new chairman of the party until Matteo Salvini took
over in 2013. Salvini changed the party name into Lega per Salvini Premier. Scholars agree that under
Salvini’s leadership the party expanded at the national level and became more radical, increasingly
emphasising more classical forms of nativism and populism. Despite this radicalization, the party can
be classified as populist and far right since its conception. In the 1994, 2001, 2008, 2018 and 2022
general elections the LN/L ran in alliance with Berlusconi’s coalitions.
\n\n
The electoral list known as Liberi e Uguali (Free and Equals, LeU) was established in 2013,
comprising the political parties \"Article 1\" and \"Italian Left\". In 2021 LeU appointed a minister in
Prime Minister Mario Draghi's government and remained in Parliament before the 2022 elections
when the two main components separated and LeU disappeared. Classifying LeU as a far-left party is
challenging because it included both moderate factions from the Democratic Party (Pierluigi Bersani 
and Roberto Speranza) and more radical factions from the Italian Left (Nicola Fratoianni), making it
borderline far left. LeU did not hold Eurosceptic views.
\n\n
In 2009, comedian Beppe Grillo and entrepreneur Gianroberto Casaleggio founded the Movimento 5
Stelle (Five Star Movement, M5S), which capitalises on the public's discontent and distrust in
politics. The party can be classified as populist given its flamboyant and anti-establishment rhetoric.
After initially forming a government with the far-right League (2018-2019), the M5S joined an
alliance with the centre-left Democratic Party. The M5S has previously endorsed Eurosceptic
stances, but it supported the pro-EU Draghi government in 2021 before withdrawing its support in
July 2022. Since the election of Giuseppe Conte as chairman in 2021, the M5S has adopted a clearer
and more progressive socioeconomic and cultural profile, which was evident during the 2022
election campaign.
\n\n
### Parties not represented in parliament:
\n\n
Alleanza Nazionale (National Alliance, AN) was founded in 1995 from a splinter within the MSI and
dissolved in 2009, when it merged in Berlusconi’s coalition The People of Freedom (PDL). Its leader
was Gianfranco Fini. Despite AN's historical and cultural links to the MSI, Gianfranco Fini had
progressively adopted a liberal conservative approach, distancing AN from the fascist legacy, with
the goal of transforming AN into a credible governing party. Overall, AN was far right even if in the
last years of its existence this classification was more dubious. Initially blatantly Eurosceptic, AN
gradually moderated its stances but continued to express criticism towards the EU.
\n\n
La Destra - Fiamma Tricolore (The Right - Tricolour Flame, LD-FT) was a coalition merging two farright parties: La Destra (The Right), headed by Francesco Storace, and Fiamma Tricolore (Tricolour
Flame). It was created in the run-up to the 2008 general elections and it was dissolved in 2009. LD-FT
was a far-right party emphasizing nativism and authoritarianism. Classifying it as populist is more
complex, as the coalition included extreme-right elements (Luca Romagnoli) openly emphasising the
principles of Italian Fascism. Whether extreme-right parties can be considered populist is still a
matter of debate among scholars, hence we consider LD-FT a borderline case of populism. LD-FR was
Eurosceptic.
\n\n
La Rete - Movimento per la Democrazia (The Network- Movement for Democracy, LR) was a
political party founded in 1991 by antimafia activist Leoluca Orlando and dissolved in 1999. LR
included former members from the Christian democratic and the communist party. The platform
was mostly centred around anti-Mafia and anti-corruption, calling to end parliamentary immunity,
to expand judicial powers to counter the Mafia, and to reduce the number of MPs in parliament. LR
described itself as a \“civic movement\". It was populist but not Eurosceptic.
\n\n
The Lega d’Azione Meridionale (Southern Action League, LAM) was founded in 1992 by Giancarlo
Cito, who was expelled from the MSI for his extreme views. Cito later became a member of the
group of the Federalisti alla Camera (created in 1994). The group supported the Berlusconi I
government until its demise in January 1995, but without any ministers or officials. In 2014, LAM
joined FI, but it later became autonomous again. The party campaigns heavily on anti-immigration
and law & order policies. LAM managed to obtain 1 seat in parliament in 1994 (Maggioritario),
thanks to the popularity of his leader in his hometown Taranto. Since then, it remains a fringe party
at the national level, mostly active in Apulia. The party is far-right, and it stands out for its bold antiLN campaigns. LAM can be classified as borderline populist as it also includes more extreme-right
elements. LAM is Eurosceptic.
\n\n
Lista Di Pietro - Italia dei Valori (Di Pietro’s list - Italy of Values, IdV) is a party founded in 1998 by
former Mani Pulite prosecutor Antonio Di Pietro. Although the party was overshadowed by the
emergence of the Five Star Movement, which also campaigned on similar issues, IdV still exists. In
the 2022 elections, IdV joined the electoral alliance Noi Moderati (We the Moderates) and elected 7
MPs and 2 Senators. The party is considered populist but does not hold Eurosceptic views.
\n\n
The Movimento Sociale Fiamma Tricolore (Social Movement Tricolour Flame, MSFT) was
established in 1995 by Pino Rauti and other more extreme members of the Italian Social Movement.
These individuals refused to align with National Alliance and refused to disregard the legacy of
historical Italian Fascism. MSFT appointed two senators in 1996 and 2001 as part of larger coalitions.
In the 2018 election, MSFT joined the Italia agli Italiani (Italy to Italians) list, which included Fiamma
Tricolore and Forza Nuova. MSFT closely associates itself with the legacy of the Italian Social
Republic (RSI) and Third Positionist ideology. It can be classified as an extreme and racist party falling
in the neo-fascist spectrum, and hence as a far-right party.. Whether extreme-right parties can be
considered populist is still a matter of debate among scholars, hence we consider MSFT a borderline
case of populism. Furthermore, MSFT is Eurosceptic.
\n\n
The Movimento Sociale Italiano - Destra Nazionale (Italian Social Movement - National Right, MSIDN) was founded in 1946 by supporters nostalgic of former dictator Benito Mussolini. It ceased to
exist in 1995 when it was transformed into the National Alliance (AN). The MSI-DN emphasised
traditional social values, law and order, and hostility towards migrants. MSI-DN has openly
emphasised its rooting in Italian Fascism. It can be classified as an extreme and racist party falling in
the neo-fascist spectrum. Whether extreme-right parties can be considered populist is still a matter
of debate among scholars, hence we consider MSI-DN a borderline case of populism. MSI-DN was far
right and Eurosceptic.
\n\n
The Partito dei Comunisti Italiani (Party of Italian Communists, PDCI) was founded in 1998,
following an internal division within Partito della Rifondazione Comunista (see above). It dissolved in
2016. According to the party statute, it aimed to “transform Italy into a socialist society based on
political democracy”. PDCI supported the D’Alema I and Amato II governments (in 1998 and 2001)
BUT by then the party had not received parliamentary representation as a result of the elections
(was a splinter of PRC). At subsequent elections, the PDCI obtained representation as an
independent party. PDCI can be classified as a far-left party as it interpreted society along a Marxist
lens but not populist. PDCI endorsed Eurosceptic stances.
\n\n
The Partito della Rifondazione Comunista (Communist Refoundation Party, PRC) was founded in
1991. After 2006, PRC ran for the general elections in coalitions with La Sinistra l’Arcobaleno, Lista
Comunista e anticaptalista, Rivoluzione Civile, Potere al Popolo and La Sinistra. PRC is mostly inspired
by Marxist philosophy and pursues the realisation of a modern socialist society, opposing neoliberalism, exalting the anti-fascist Resistance, and supporting the democratic constitution. The party
openly opposes authoritarian, bureaucratic and “Stalinist degenerations” of socialism. PRC is far left
but not populist. PRC is Eurosceptic.
\n\n
Rivoluzione Civile (Civil Revolution, RC) was founded in 2012 by Antonio Ingroia, a former anti-mafia
prosecutor. RC dissolved in 2013, after the general elections when the party obtained 2.2% of the
vote but no seats. RC’s ideological platform was inspired by anti-corruption policies, communism,
and environmentalism. RC was far left and populist in that it integrated several ideological tropes
from the former Italia dei Valori (giustizialismo against ‘corrupt elites and a simplified vision of
society). RC referred to a poorly defined ‘civil society’ that comes closer to the definition of ‘the pure
people’ used in the PopuList. RC was not Eurosceptic.
\n\n
Sinistra Ecologia e Libertà (Left Ecology and Freedom, SEL) was a party founded in 2009 by Nichi
Vendola. In 2015, SEL joined the newly born parliamentary group Italian Left (Sinistra Italiana, SI),
and the party dissolved in 2016. SEL’s worldview was inspired by democratic socialism. It opposed
capitalist economy and called for peace, non-violence, social justice, and ecological transition of the
economy and society. SEL was far left but not populist. SEL is not Eurosceptic.
  ",
# Latvia ----
  "
\n\n
### Parties represented in parliament:
\n\n
Latvija Pirmajā Vietā (Latvia First, LPV) is a populist right-wing party founded in 2021 by Ainārs
Šlesers. The party is nationalist and socially conservative: it stands for economic protectionism and
personal freedom, and supports (large and natural) Latvian families as well as Christian values. The
party came to prominence for its anti-establishment and anti-vax stances during the COVID-19
pandemic and underwent internal turmoil amid Russia’s invasion of Ukraine; outgoing members
went on to form the party Sovereign Power. LPV returned 6.2% of the vote in the 2022 election,
returning 9 MPs.
\n\n
The Nacionālā Apvienība (National Alliance, NA; short for National Alliance “All for Latvia!” – “For
Fatherland and Freedom/LNNK”) is a far-right party founded in 2010 from the merger of the far-
right All for Latvia! and For Fatherland and Freedom/LNNK. The party is anti-immigrant, socially
conservative, pro-market, anti-Russian, pro-NATO, and Eurosceptic. The party peaked at 16.7% of
the vote in 2014 and has joined all right-wing coalitions since 2011 to prevent government formation
by the social-democratic coalition. NA returned 9.3% of the vote in the 2022 election.
\n\n
### Parties not/no longer represented in parliament:
\n\n
Dzimtene (Motherland; currently known as New Harmony, JS) is a marginal populist far-left party
founded in 2004. The party is anti-establishment, democratic socialist, and syndicalist. Dzimtene
peaked at 2.1% of the vote in the 2006 election. The party underwent several name changes and
incarnations and contested the 2018 election as For an Alternative (Par Alternatīvu), returning 0.3%
of the vote.
\n\n
Jaunais Laiks (New Era, JL) was a populist right-wing party founded in 2002 by financier Einars
Repše. The party was anti-establishment, anti-bureaucracy, anti-corruption, and economically
liberal. JL gained 24.0% of the vote in the 2002 election and led a short-lived government between
2002 and 2004. The party returned to government in 2004 but the new coalition was tainted by
scandals which led to JL ministers’ resignation. The party joined another government coalition in
2009. In 2010, JL returned 31.9% of the vote as part of the Unity alliance, also comprising the
conservative Civic Union and Society for Political Change. The three parties dissolved and merged
into Unity in 2011.
\n\n
Par Cilvēcīgu Latviju (For a Humane Latvia, PLC; formerly known as Kam Pieder Valsts?, Who Owns
the State?, KPV LV) is a populist right-wing party founded in 2016 by actor and radio host Artuss
Kaimiņš. The party is anti-corruption, anti-establishment, social conservative, and Eurosceptic. KPV
LV came second in the 2018 election with 14.3% of votes and formed a government coalition with
the conservative New Conservative Party and New Unity, the far-right National Alliance as well as
the liberal Development/For!. KPV LV has been marred by infighting, which led to several defections
and expulsions (including Kaimiņš’s), and a name change to ‘For a Humane Latvia’. The dire state of
PLC was confirmed in the 2022 election, when the party returned a meagre 0.3% of the vote.
\n\n
Katram un Katrai (For Each and Every One, KuK; formerly known as Law and Order, LuK) is a
populist right-wing party founded in 2021 by Aldis Gobzems, former member and PM candidate for
Who Owns the State? The party is anti-establishment, nationalist, socially conservative, and
Eurosceptic, and places significant emphasis on a strong family and individual entrepreneurship. The
party came to prominence during the COVID-19 pandemic for its anti-vax and anti-restriction
positions. KuK returned 3.7% of the vote in the 2022 election and thus failed to cross the electoral
threshold. As a result, Gobzems resigned from chairmanship and quit the party.
\n\n
The Latvijas Komunistiskā Partija (Communist Party of Latvia, LKP) was a far-left party founded in
1904 and a government party during the Soviet occupation of Latvia. This Marxist-Leninist party
contested the first free elections of 1990, scoring 21.5% of the vote. The KLP was banned in 1991.
The Latvijas Sociālistiskā Partija (Socialist Party of Latvia, LSP) is a marginal far-left party founded in
1994 as the successor of the banned Communist Party of Latvia. The party is communist, anticorruption, Eurosceptic, and is perceived to serve the interests of the Russian-speaking population.
The party joined the social-democratic alliance Harmony Centre in 2005.
\n\n
The Latvijas Vienības Partij (Latvian Unity Party, LVP) was a far-left party founded in 1992. The
party was democratic socialist and anti-corruption, and mainly concerned with national 
independence, employment, industrial renewal, and rural development. The LVP peaked at 7.1% of
the vote in the 1995 election, but failed to re-enter parliament in 1998 and dissolved in 2001.
\n\n
The Reformu Partija (Reform Party, RP) was a short-lived populist right-wing party formed in 2011
by former president of Latvia Valdis Zatlers. The party was anti-establishment. The RP gained 20.8%
of the vote in the 2011 election and formed a government coalition with the right-wing Unity and
the far-right National Alliance. The party dissolved in 2015 and most of its members joined Unity.
Suverēnā Vara (Sovereign Power, SV) is a populist right-wing party founded in 2022. The party is
anti-establishment, nationalist, economically protectionist, socially conservative, placing a strong
accent on direct-democratic measures and freedom of speech, re-emigration, and the natural family.
The party returned 3.2% of the vote in the 2022 election, failing to return any MP.
\n\n
Tēvzemei un Brīvībai (For Fatherland and Freedom, TB) was a populist far-right party founded in
1993 which merged with the Latvijas Nacionālās Neatkarības Kustība (Latvian National
Independence Movement, LNNK) in 1997 to form the TB/LNKK. The party was anti-communist,
socially conservative, economically liberal, and Eurosceptic. The party peaked at 14.7% of the vote in
the 1998 election. In 2010, the party contested elections with the far-right All for Latvia! (VL). The
two parties officially merged in the National Alliance in 2011.
\n\n
The Tautas Kustība Latvijai (People’s Movement for Latvia, TKL; also known as Siegerist Party) was
a short-lived populist far-right party founded in 1994 by German-Latvian journalist and former
Latvian National Independence Movement (LNNK) MP Joachim Siegerist. The party was socially
conservative, anti-communist, anti-corruption, and pro-European, and campaigned on the platform
‘Russians to Russia and Latvia for Latvians’. The TKL scored 14.9% of the vote in the 1995 election,
returning 16 MPs. The party split in 1996 and failed to enter parliament again in 1998.
  ",
# Lithuania ----
  "
### Parties represented in parliament
\n\n
The Darbo Partija (Labour Party, DP) is a centrist party founded in 2003 by entrepreneur Viktor
Uspaskich. The party is a former case of populism (from its establishment until 2006), and is
reformist, economically liberal, and has voiced anti-immigration views and opposition to the EU
relocation and resettlement scheme in the midst of the asylum policy crisis. The DP aspires to bring
together and elect honest and hard-working people, and create a system of MP recall to make
representatives accountable. The DP peaked at 28.4% of votes in the 2004 election and took part in
different government coalitions over the years. The party scored 9.8% of the vote in the 2020
election.
\n\n
### Parties not/no longer represented in parliament
\n\n
The Drąsos Kelias (Way of Courage, DK) is a populist party founded in 2012. The party is
conservative and anti-corruption. DK gained 8.3% of the vote in 2012 but many members left the
party during the legislature. The party was marred by defections and quickly lost support . DK scored
1.2% of the vote in the 2020 election.
\n\n
The Fronto Partija (Front Party, FRONTAS) was a short-lived far-left party founded in 2008. The
party was democratic socialist, statist, and Eurosceptic. FRONTAS scored 3.2% of the vote in the
2008 election and merged with the Lithuanian Socialist Party into the Socialist People’s Front in
2009.
\n\n
Jaunoji Lietuva (Young Lithuania, JL) is a marginal far-right party founded in 1994 and led by
Stanislovas Buškevičius. The party is anti-Russian, social conservative, and Eurosceptic. The party
scored its best performance in the 1996 election, when it returned 4.0% of the vote. JL failed to
enter parliament after 2004.
\n\n
The Lietuvos Centro Partija (Lithuanian Centre Party, LCP) is a populist right-wing party founded in
2003. The party is nationalist, economically liberal, and Eurosceptic. The Centre Party prioritises
national interests and identity over European ones. The Centre Party peaked at 6.1% of the vote as
part of the Anti-corruption coalition of Kristupas Krivickas and Naglis Puteikis. The party gained 2.4%
of votes in the 2020 election, which were contested under the name Centre Party – Nationalists, in
alliance with the far-right Lithuanian Nationalist and Republican Union (LTS).
\n\n
The Lietuvos Laisvės Sąjunga (Lithuanian Freedom Union, LLS) was a marginal far-right party
founded in 1992 by Vytautas Šustauskas and split from the Lithuanian Freedom League. The party
showed signs of extremism, expressing anti-Semitic and Eurosceptic views. The LLS scored its highest
result in 1996 (1.5% of the vote and no MPs) but managed parliamentary representation (1 MP,
Šustauskas himself, elected in the single-member Šilainiai constituency) between 2000 and 2004.
The party dissolved in 2011.
\n\n
The Lietuvių Tautininkų ir Respublikonų Sąjunga (Lithuanian Nationalist and Republican Union,
LTS; also known as Nationalists) is a far-right party founded in 1990 and claiming historical
continuity with the Lithuanian Nationalist Union. The LTS is anti-immigration, anti-Russian, social
conservative, anti-globalist, and hard Eurosceptic. The current name derives from the merger with
the Republican Party, which occurred in 2017. The Nationalists delivered 4 MPs in the 1992 election
but scored their best result in 2020 (2.4% of the vote) as part of the Centre Party – Nationalists
alliance, which however failed to return any MP.
\n\n
The Nacionalinis Susivienijimas (National Alliance, NS) is a Eurosceptic party founded in 2020 by
philosopher Vytautas Radžvilas, founder and former leader of the Lithuanian Liberal Union. The
party is nationalist and borderline far right, social conservative, economically protectionist, and
Eurosceptic. The NS scored 2.2% of votes in the 2020 election and delivered no MPs.
\n\n
The Tautos Prisikėlimo Partija (National Resurrection Party, TPP) was a short-lived populist centrist
party founded in 2008 by TV host and producer Arūnas Valinskas. The party was anti-establishment
and economically liberal. Thanks to the visibility afforded by the presence of media celebrities in its
ranks, the party scored 15.1% of the vote in the first election contested in 2008 and took part in the
government including the Conservatives and the Liberal Movement. The TTP was however marred
by defections and eventually merged into the Liberal and Centre Union in 2011.
\n\n
Tvarka ir Teisingumas (Order and Justice, TT) was a populist right-wing party founded in 2002 as a
breakaway from the Liberal Union of Lithuania. The party was nationalist, social conservative, and
Eurosceptic. TT’s fate has been tied to former chairman and Lithuanian president Rolandas Paksas,
who was impeached in 2004 and eventually resigned from party leadership in 2016. TT peaked at
12.7% of the vote in the 2008 election, which allowed the party to join the government with the 
Labour Party and the Social Democratic Party. TT successively experienced a gradual decline until the
eventual merger into Freedom and Justice in 2020.
  ",
# Luxembourg ----
  "
### Parties represented in parliament
\n\n
The Alternativ Demokratesch Reformpartei (Alternative Democratic Reform Party, ADR) was
founded in 1987 as a single-issue pensioners’ party. After the government had implemented many of
the pension reforms advocated by the party, the ADR started to embrace a more virulent
Euroscepticism and a more outspoken anti-elitist discourse. For this reason the party is categorised
as populist. Although the ADR also criticises Islam and multiculturalism, it has long been relatively
moderate in this respect. New leader Fred Keup, elected in 2022, is more radical on these issues.
Therefore, ADR is classified as borderline far right from 2022 onwards.
\n\n
Dei Lenk (The Left) was formed in 1999 as an alliance of several far-left parties. The party combines
anti-capitalism and Euroscepticism with a green outlook when it comes to environmental issues and
is therefore classified as both far left and Eurosceptic. The party received 5.5% of the votes during
the national elections in 2018.
\n\n
### Parties not/no longer represented in parliament
\n\n
The Kommunistesch Partei vu Lëtzebuerg (Communist Party of Luxembourg, KPL) was founded in
1921. In 1999 the party was involved in founding Dei Lenk, and in that year’s national elections, the
KPL did not participate with a separate list. After the 1999 elections, the party did not obtain
parliamentary seats anymore. The party was far left and Eurosceptic.
\n\n
The National Bewegong (National Movement, NB) was a far-right party that never managed to
obtain parliamentary seats
  ",
# Netherlands ----
  "
### Parties represented in parliament
\n\n
The BoerBurgerBeweging (Farmer–Citizen Movement, BBB) was founded in 2019 as a response to
widespread farmers’ protests against nitrogen emission restrictions. The party obtained one seat in
the 2021 elections, and went on to win the 2023 provincial elections with one-fifth of the vote
nationwide. It can be classified as a populist party, defending the “gewone Nederlanders” (ordinary
Dutchmen) against the establishment from the cosmopolitan cities. Moore specifically, it can be
classified as an agrarian populist party, presenting itself as a party that stands up for the interests of
farmers, but also people from the countryside in general. It glorifies the countryside’s norms and
values and traditions, and the common sense of its inhabitants. Because of its very restrictive
positions on immigration, a theme on which the party consistently votes with the other far right
parties in the Netherlands, it has also been classified as borderline far right.
\n\n
The Forum voor Democratie (Forum for Democracy, FVD) was founded in 2015 by Thierry Baudet.
The party won 1.8% of the vote in the 2017 national election and subsequently experienced
exponential growth, becoming the largest party in the provincial elections of 2019. By the 2021
elections, its support had been reduced again to 5% of the vote. Since its foundation, the party has
been classified as a populist and far-right party. The party rejects the “cultural-marxist” elite and its
institutions and propagates the great replacement theory. Since 2017 the party has radicalised
significantly, espousing conspiracy theories, pushing antisemitism, and arguing for the establishment
of a countersociety that no longer obeys the state. For this reason, the party is these days no longer
considered to be a radical right party by country experts. Instead, they now classify the party as
extreme right.
\n\n
The Juiste Antwoord 21 (Right Answer 21, JA21) was founded in 2021 by Joost Eerdmans and
Annabel Nanninga after the elected politicians left the FvD. In the 2021 elections, JA21 entered
parliament with 2.4% of the votes. Although the party presents itself as a respectable alternative to
FVD and PVV, it can nevertheless be classified as populist and far right on the basis of its discourse,
programmatic stances, and parliamentary voting behaviour.
\n\n
The Partij voor de Vrijheid (Party for Freedom, PVV) was formed by a former MP of the
conservative liberal VVD, Geert Wilders, in 2006. It won 6% of the vote in 2006, increasing its vote
share to 15% in 2010. After the 2010 election, the party supported a minority cabinet consisting of
the centre-right VVD and CDA, but it withdrew its support in 2012. In recent elections, the support
for the party has been between 10% and 13% of the vote. Although Wilders came from the political
mainstream, his party can be classified as populist and far right since its inception. It campaigns
against the “left-wing Church” that has enforced multiculturalism on the ordinary Dutch. The PVV
puts particular emphasis on Islamophobia, arguing that Islam is a totalitarian ideology that seeks to
undermine Western democracies.
\n\n
The Socialistische Partij (Socialist Party, SP) was founded as a Maoist party in 1971. After scrapping
the term Marxism-Leninism, the party entered parliament in 1994 with 1.3% of the votes. In 2006
the party obtained its best result ever: 16.6% of the vote. In more recent years, the party has been in
electoral decline. In its formative years, the party can be classified as populist. During this period, the
campaign slogan was strongly populist: “Vote Against, Vote SP”. In 2002 the slogan was changed to
“Vote For, Vote SP” to indicate that it was not merely an anti-establishment actor and the party
became less populist. For this reason the party is classified as borderline populist from this moment
onwards. It is also classified as borderline far left from the moment it became a democratic socialist
party in the early 1990s.
\n\n
### Parties not/no longer represented in parliament
\n\n
The Centrum Democraten (Centre Democrats, CD) were founded by Hans Janmaat in 1984 as a
successor to the Centrumpartij (Centre Party, CP). It received 0.9% of the vote in 1989 and 2.5% in
1994. The CD lost all its seats in 1998 and was dissolved in 2002, shortly after Janmaat’s death. It has
been classified as populist and far right. The party was known for its anti-immigration stances and its
heavy criticism of multiculturalism and the ‘political correctness’ of the mainstream parties.
Leefbaar Nederland (Livable Netherlands, LN) was founded In 1999 by several media personalities.
It grew out of several local leefbaar initiatives, which were rather successful anti-establishment
movements in municipal elections. The party obtained only 1.6% of the vote in the 2002 elections,
which were lost in 2003. The party can be classified as populist on the basis of its appeal to the
ordinary people and its anti-establishment discourse.
\n\n
The Lijst Pim Fortuyn (List Pim Fortuyn, LPF) was founded by Pim Fortuyn shortly before the 2002
elections. Although Fortuyn was murdered 9 days before the elections, the LPF received 17% of the
vote in 2002. It participated in a government coalition with the conservative liberals (VVD) and
Christian democrats (CDA), which fell apart after three months. In subsequent elections, the party
gradually lost all of its support. Although the party campaigned heavily on immigration and
integration, its stances on these issues were relatively moderate, in particular with regard to
immigrants and Muslims. We therefore classify the party as populist, and not as far right.
  ",
# Norway ----
  "
### Parties represented in parliament:
\n\n
The Fremskrittspartiet (Progress Party, FrP) is borderline far right, populist and soft Eurosceptic. The
party was founded by Anders Lange in 1973 as an anti-tax party. Lange was openly racist and
supported Apartheid in South Africa. In the late 1980s, the party politicised the immigration issue
under Carl I. Hagen’s leadership and thereby became more similar to other European far right
parties – yet still relatively moderate compared to these other parties. We have therefore classified
the party as borderline far right. In 2006, Siv Jensen became the new party leader. She was seen as
more professional and less confrontational than Hagen. In 2009, she managed to attract almost a
quarter of the electorate. The party was in government between 2013 and 2020.
\n\n
Rødt (Red Party) was founded in 2007 as a merger of two communist parties, which both had been
founded in 1973. The party is a far-left party, endorsing a revolutionary ideology, aiming for a
classless society. Rødt is also strongly Eurosceptic. Despite the hard left and revolutionary principles,
the party is against the use of violence. In fact, the current leader, Bjørnar Moxnes, wants to get rid
of the party’s ideological commitment to communism. Instead, he often employs populist rhetoric to
appeal to supporters. We therefore label the party borderline populist since Moxnes was elected
leader in 2012. In the 2021 parliamentary elections, Moxnes led the party to its best result ever
(4.7%).
\n\n
In 1973, the Sosialistisk Venstreparti (Socialist Left Party, SV) was founded in 1973 as an electoral
coalition consisting of more and less radical left parties. Two years later, the party was turned into a
real, unified political party. In the late 1980s, SV gradually turned into a pragmatic new left party,
mobilising particularly on issues like the environment and education. We classify the party therefore
as borderline far left. Just like Rodt, the party is strongly Eurosceptic. Between 2005 and 2013, SV
participated in government coalitions with the Labor Party and the agrarian Center party. The
current leader is Audun Lysbakken. The party obtained about 8% of the votes during the elections in
2021.
\n\n
### Parties not/no longer represented in parliament
\n\n
The Kystpartiet (Coastal Party, Kp) is a very small national conservative party with a clear regionalist
profile attracting votes mostly from the north of the country. The party is mildly populist and
Eurosceptic, and is currently not represented in parliament. The party had one MP between 1997
and 2005. Because its populism was mild, we categorise the party as borderline populist.
  ",
# Poland ----
  "
### Parties represented in parliament
\n\n
Konfederacja Wolność i Niepodległość (Confederation Freedom and Independence, Konfederacja)
is a far-right alliance founded in 2018 and including the monarchist Konfederacja Korony Polskiej
(Confederation of the Polish Crown, Korona) led by Grzegorz Braun, the economically liberal and
socioculturally conservative Konfederacja Odnowy Rzeczypospolitej Wolność i Nadzieja (Coalition
for the Renewal of the Republic – Freedom and Hope, KORWiN) led by Janusz Korwin-Mikke, and
the extremist Ruch Narodowy (National Movement, RN) led by Robert Winnicki. The alliance
campaigned on an anti-immigration, anti-LGBTQI+, social conservative, economically liberal, and
Eurosceptic agenda, and was very critical of restrictions during the COVID-19 pandemic.
Konfederacja returned 6.1% of the vote in the 2019 election and its candidate Krzysztof Bosak (RN)
scored 6.8% of the vote in the 2020 presidential election.
\n\n
Kukiz’15 is a populist right-wing party founded in 2015 by singer and actor Paweł Kukiz. The party is
anti-establishment, anti-corruption, social conservative, and Eurosceptic, and advocates direct
democracy. The Kukiz’15 party list in the 2015 election included, among others, members of the farright National Movement, which however ended cooperation with Kukiz in 2016. The party scored
8.8% of the vote in 2015 and 8.6% in 2019 (the latter score as part of the Polish Coalition led by the
agrarian and conservative Polish People’s Party). Since 2021, Kukiz’15 has provided support to the
government led by the far-right Law and Justice.
\n\n
Lewica Razem (Left Together, LR; formerly known as Razem, Together) is a far-left party founded in
2015. The party stands for democratic socialism, and is economically and socially progressive,
endorsing redistribution, public ownership of services, drug liberalisation, LGBTQI+ rights, and
environmentalism. LR is pro-EU and anti-imperialist, and has been a vocal supporter of Ukraine and
its independence in the face of the Russian invasion of the country in 2022. LR scored 3.6% of the
vote in the 2015 election. As of 2019, it is part of The Left (Lewica) coalition alongside New Left. The
coalition returned 12.6% of the vote in the 2019 election.
\n\n
Prawo i Sprawiedliwość (Law and Justice, PiS) is a populist far-right party founded in 2001 by twin
brothers Lech and Jarosław Kaczyński. The party started out as a conservative force but steered
towards the far-right end of the ideological spectrum since the late 2000s. Therefore, it should be
considered borderline populist and borderline far right between 2005 and 2015, and fully part of the
sets thereafter. PiS is social conservative, economically protectionist and statist, anti-immigration,
anti-Russian, and Eurosceptic. The party led a short-lived government coalition with the populist
Samoobrona (Self-Defence) and the far-right League of Polish Families (2006-2007) after securing
27.0% of the vote in the 2005 election. Lech Kaczyński served as president of Poland from 2005 until
his death in the crash of a Polish Air Force jet in 2010. PiS returned to power in 2015 (37.6% of the
vote) and improved on its performance in 2019 (44.3% of the vote) leading the United Right
coalition. Jarosław Kaczyński has acted as informal head of the government throughout. Since 2015,
PiS has set the country on an illiberal trajectory, capturing the judiciary, usurping constitutional
bodies, attacking liberal civil society, and imposing severe restrictions on abortion access.
\n\n
Solidarna Polska (United Poland, SP) is a populist far-right party founded in 2012 by Zbigniew Ziobro
as a split from Law and Justice (PiS). The party is social conservative, economically statist, antiimmigration, and Eurosceptic. Since 2014, SP has contested elections with Law and Justice as part of
the United Right ticket and has thus sat in government between 2015 and 2019, and again since
2019, when the coalition peaked at 44.3% of the vote.
\n\n
### Parties not/no longer represented in parliament:
\n\n
Kongres Nowej Prawicy (Congress of the New Right, KNP) is a marginal far-right party founded in
2011 by Janusz Korwin-Mikke. The party is social conservative, economically liberal, monarchist, and
hard Eurosceptic. Korwin-Mikke was ousted from the party in 2015 and went on to establish 
KORWiN. The party fielded candidates as part of the Kukiz’15 party list in the 2015 election and
delivered 1 MP.
\n\n
The Liga Polskich Rodzin (League of Polish Families, LPR) was a former populist far-right party (until
2010) founded in 2001 by lawyer Roman Giertych. The party used to be anti-establishment, social
conservative, anti-globalist, and Eurosceptic. The LPR entered parliament in 2001 with 7.9% of the
vote and in 2005, when it returned 8.0% of the vote. The party was then asked to join a short-lived
government coalition led by Law and Justice and also including the populist Samoobrona (SelfDefence). Giertych was appointed Minister of National Education during this term. The LPR failed to
re-enter parliament in 2007 and became politically irrelevant thereafter. During the 2010s, the party
transformed into a moderate, pro-EU political force.
\n\n
Partia X (Party X, X) was a marginal populist far-right party founded in 1990 by businessman and
former presidential candidate Stanisław Tymiński. The party ran on a nationalist, conservative, and
anti-globalist platform. Party X returned 3 MPs in the 1991 election with just 0.5% of the vote. It
improved its performance in 1993 (2.7% of the vote) but failed to cross the electoral threshold
recently introduced. The party dissolved in 1999.
\n\n
The Polski Związek Zachodni (Polish Western Union, PZZ) was a marginal far-right organisation
historically established in 1934 and revived in 1989. The organisation mobilised on ‘Recovered
Territories’ (i.e. former eastern territories of Germany and the city of Gdańsk). The party contested
the 1991 election returning 0.2% of the vote and delivering 4 MPs.
\n\n
Samoobrona Rzeczpospolitej Polskiej (Self-Defence of the Republic of Poland, Samoobrona) was a
populist party founded in 1992 by Andrzej Lepper. The party was agrarian, nationalist, economically
statist and protectionist, in certain respects culturally liberal, and Eurosceptic. Samoobrona
remained at the margins of electoral politics until 2001, when it entered parliament with 10.2% of
the vote, often providing support to the government led by the social-democratic Democratic Left
Alliance. The party gained 11.4% of the vote in the 2005 election, after which it was asked to join the
short-lived government led by Law and Justice alongside the far-right League of Polish Families
(2006-2007). Lepper was appointed Minister of Agriculture, but was later dismissed from office by
PM Jarosław Kaczyński on allegations of corruption, instigating an early election. Samoobrona’s
support plummeted and was not able to re-enter parliament again. Lepper committed suicide in
2011 and the party disappeared into oblivion.
\n\n
The Unia Polityki Realnej (Real Politics Union, UPR) is a far-right party founded in 1987 by Janusz
Korwin-Mikke. The party is social conservative, economically liberal, and Eurosceptic. In 2009,
Korwin-Mikke left the party and later went to form the Congress of the New Right. Between 2012
and 2015, the URP teamed up with the extremist All-Polish Youth and the National Radical Camp to
form the National Movement, eventually fielding candidates within the Kukiz’15 party list. The URP
however left the National Movement in 2015 and terminated cooperation with Kukiz in 2019. At the
electoral level, the party scored 3.2% of the vote in the 1993 election and delivered MPs as part of
the conservative Civic Platform (2001) and populist Kukiz’15 (2015) party lists.
\n\n
The Zjednoczenie Chrześcijańsko-Narodowe (Christian National Union, ZChN) was a populist farright party founded in 1989 by then Sejm Marshal Wiesław Chrzanowski. The party was social
conservative and economically statist and protectionist. The ZChN was a member of the Catholic
Electoral Action alliance, which scored 8.8% of the vote in 1991. Upon establishment of Law and
Justice in 2001, a number of ZChN members left to join the new party. Other members left ZChN to
join the far-right League of Polish Families in 2007. The party eventually dissolved in 2010.
\n\n
The Ruch Odbudowy Polski (Movement for the Reconstruction of Poland, ROP) was a borderline
populist far-right party founded in 1995 by former PM of Poland and presidential candidate Jan
Olszewski. The party was nationalist, anti-communist, anti-German, and Eurosceptic. The ROP gained
5.6% of the vote in the 1997 election and managed to deliver MPs in the 2001 and 2007 elections as
part of the League of Polish Families and Law and Justice party lists. The party dissolved in 2012.
  ",
# Portugal ----
  "
### Parties represented in parliament
\n\n
The Bloco de Esquerda (Left Bloc, BE) was founded in 1999 as a merger of several communist
parties. From the outset, the party presented itself as strongly anti-establishment. Over the years, BE
has become less radical and has increasingly embraced green and progressive positions. The party
can be categorised as borderline far left. In the European elections of that year, the BE has not
presented itself as Eurosceptic anymore. The BE supported a minority coalition between 2015 and
2021, which fell after the party withdrew its support. The BE did not perform well during the
following elections, obtaining only 4.4% of the vote.
\n\n
Chega (Enough) was founded in 2019 by André Ventura, a former law professor and sports
commentator. Chega is a populist and far-right party, strongly emphasising cultural issues like
immigration, Islam, and “political correctness”. The party can also be categorised as Eurosceptic.
Chega obtained only 1.3% of the vote in the 2019 parliamentary elections, but increased its vote
share to 7.2% in the subsequent elections in 2022.
\n\n
The Partido Comunista Português (Portuguese Communist Party, PCP) was founded in 1921. It has
participated in elections as part of the Coligação Democrática Unitária (Unitary Democratic Coalition,
CDU), an electoral coalition that also included the ecologist party (PEV). The PCP is a far left party,
which strongly espouses Marxist principles. The PCP supported a minority coalition between 2015
and 2021 and moderated its core socioeconomic positions. The party moderated its core
socioeconomic positions as part of supporting a minority coalition between 2015-2021. The party
lost seats during the subsequent elections in 2022.
  ",
# Romania ----
  "
### Parties represented in parliament:
\n\n
The Alianța pentru Unirea Românilor (Alliance for the Union of Romanians, AUR) is a populist farright party founded in 2019 and led by George Simion. The party is irredentist, anti-minority, antiimmigration, social conservative, anti-LGBTQI+, and Eurosceptic. AUR has notably advocated the
reunification of Romania and Moldova and stands for the union of all Romanian people (i.e. within
and beyond state borders). The party entered parliament in 2020 with 9.1% of the vote, mostly
thanks to the vote of the Romanian diaspora.
\n\n
The Partidul Social Democrat (Social Democratic Party, PSD) was previously named National
Salvation Front (FSN, 1990-1992), Democratic National Salvation Front (FSDN, 1992-1993), and Party
of Social Democracy in Romania (PSDR, 1993-2001). The party qualified as populist and authoritarian
between 1990 and 1996, when the PSDR started a process of ideological change culminating in the
collaboration with major social-democratic international partners in 1999 and a mainstream turn
with the election of Adrian Năstase to PM in 2000. The PSD was borderline populist between the
chairmanship of Victor Ponta (2010) and the imprisonment of Liviu Dragnea (2019). Despite being
nominally social democratic, the PSD is nationalist, economically liberal, socially conservative, and
has at times expressed Eurosceptic views. The party peaked at 58.6% of the vote in 2012 under the 
leadership of Victor Ponta. The PSD returned 28.9% of the vote in the 2020 election and has been
part of a grand coalition (National Coalition for Romania) since November 2021.
\n\n
### Parties not/no longer represented in parliament
\n\n
The Partidul Mișcarea Populară (People’s Movement Party, PMP) is a populist party founded in
2014 by former president of Romania Traian Băsescu. The party is nationalist, social conservative,
economically liberal, and pro-EU, and has progressively taken anti-immigration and anti-LGBTQI+
positions. The party returned 5.3% of the vote in the 2016 election, providing external support to the
Liberal government between 2019 and 2020. The PMP shortly missed the threshold in the 2020
election, scoring 4.8% of the vote.
\n\n
The Partidul Poporului – Dan Diaconescu (People’s Party – Dan Diaconescu, PP-DD) was founded in
2011 by TV host and media entrepreneur Dan Diaconescu shortly after his release from prison for a
case of extortion. The party was left-wing nationalist and populist. The party was short-lived and
marred by defections. The PP-DD gained 14.0% of the vote in the 2012 election and provided the
organisational infrastructure for Diaconescu’s presidential bid in 2014. Upon eventual conviction of
Diaconescu in 2015, the party merged into the National Union for the Progress of Romania in 2015.
\n\n
The Partidul România Mare (Greater Romania Party, PRM) is a populist far-right party founded in
1991 by Corneliu Vadim Tudor. The party is irredentist and anti-minority, advocating the
reconstitution of Romanian borders along those of the Kingdom of Romania and especially
denouncing the Hungarian minority in Romania as a ‘fifth column’. The party is social conservative
and Eurosceptic, and was originally nostalgic of Nicolae Ceaușescu’s communist regime. The party
gained its last parliamentary entry in 2004 (12.9% of the vote) and the party faded into electoral
marginality thereafter.
\n\n
The Partidul România Unită (United Romania Party, PRU) is a marginal far-right party founded in
2015. The party is extremist, anti-establishment, anti-minority, anti-immigration, social conservative,
economically protectionist, anti-NATO, and hard Eurosceptic. The party scored 2.8% of the vote in
the 2016 election.
\n\n
The Partidul Socialist al Muncii (Socialist Party of Labour, PSM) was a far-left party founded in 1990
by Ilie Verdeț. The party was left-wing nationalist and included many non-reformist members of the
Romanian Communist Party. The party scored 3.0% of the vote in the 1992 election and entered the
government coalition led by the Democratic National Salvation Front. The party was dissolved in
2003.
\n\n
The Partidul Socialist Democratic Român (Romanian Socialist Democratic Party, PSoDR) was a
short-lived and marginal far-left party founded in 1990 by Marius Cîrciumaru. The party was
democratic socialist and merged into the Democratic National Salvation Front in 1993.
The Partidul Unităţii Naţionale a Românilor (Romanian National Unity Party, PUNR) was a populist
far-right party founded in 1990 by Gheorghe Funar. The party was anti-minority and social
conservative. The PUNR scored 7.7% of the vote in 1992 and was asked to join the government
coalition led by the Democratic National Salvation Front and also including the far-right Greater
Romania Party. The party obtained parliamentary representation for the last time in 1996 (4.2% of
the vote) and was dissolved in 2006.
  ",
# Slovakia ----
  "
### Parties represented in parliament
\n\n
The Kotlebovci – Ľudová Strana Naše Slovensko (Kotlebists – People’s Party Our Slovakia, ĽSNS;
formerly known as People’s Party Our Slovakia) is a far-right party founded in 2010 by Marian
Kotleba from the ashes of the outlawed Slovak Togetherness movement. The party is ideologically
extremist and has anti-system, anti-minority, anti-immigration, social conservative, anti-LGBTQI+,
pro-Russian, anti-NATO, and hard Eurosceptic views. The ĽSNS made its first electoral inroads in the
Banská Bystrica region (the party’s stronghold), where Kotleba was regional governor between 2013
and 2017. The party entered parliament in 2016 with 8.0% of the vote, securing the same result in
the 2020 election. In January 2021, MPs of the ĽSNS parliamentary group left the party over the
decision to change the statute and centralise power in the hands of Kotleba. The dissenting MPs
have joined and revamped the pre-existing party Republika. In April 2022, Kotleba was convicted for
expressing extremist sympathies, losing his mandate as MP.
\n\n
Obyčajní Ľudia A Nezávislé Osobnosti (Ordinary People and Independent Personalities, OĽANO) is
a populist right-wing party founded in 2011 by Igor Matovič as a split from Freedom and Solidarity
(SaS) parliamentary faction. The party’s main campaign theme is anti-corruption and anti-elitism,
and it has a socially conservative platform. The party came third in the 2012 and 2016 elections,
gaining 8.6% and 11.0% of the vote, respectively – the last time in alliance with the conservative
New Majority. OĽANO was part of the winning ticket in the 2020 election (including the Christian
Union, New Majority, and Change from Bottom), which secured 25.0% of the vote. However, the
alliance fell short of a parliamentary majority and formed a coalition government with the far-right
We Are Family, the right-wing Freedom and Solidarity, and the centrist For the People, with Igor
Matovič as PM. Matovič resigned in March 2021, following a government crisis over the purchase of
Sputnik V COVID-19 vaccines from Russia. Matovič and Eduard Heger (also of OĽANO) traded places,
with the former being appointed Minister of Finance and the latter becoming PM of Slovakia.
\n\n
Smer (Direction) is a populist party founded in 1999 by Robert Fico, an outgoing member of the
Party of the Democratic Left. The party combines economic paternalism with nationalist elements, is
socially conservative, and generally pro-EU. Smer has been a central political force in the politics of
Slovakia throughout the 2000s and 2010s. The party led government coalitions in 2006 (29.1% of the
vote) and 2016 (28.3% of the vote) with the far-right Slovak National Party, among others. In 2012,
Smer gained sufficient seats to rule on its own after gaining 44.4% of the vote. The party was among
the Visegrád-4 governing forces that rejected the EU migrant relocation and resettlement scheme.
Smer has been often embroiled in cases of corruption. The political crisis following the murder of
investigative journalist Ján Kuciak and his fiancée led to the resignation of PM Fico and his cabinet in
March 2018. Peter Pellegrini (also from Smer) formed a new government, which remained in place
until 2020. Smer gained 18.3% of the vote in the 2020 election. Pellegrini left Smer in 2020 to form
Hlas (Voice).
\n\n
Sme Rodina (We Are Family, SR) is a populist far-right party founded in 2015 by Boris Kollár and
previously known as the Party of Citizens of Slovakia. The party is socially conservative, antiestablishment and anti-corruption, anti-immigration, and Eurosceptic. SR entered parliament in
2016 after securing 6.6% of the vote and improved its performance in 2020, when it gained 8.2% of
the vote. The party was then asked to join the government coalition led by Igor Matovič’s populist
right-wing OĽANO with one of the SR vice-presidents, Milan Krajniak, appointed as Minister of
Labour.
\n\n
### Parties not/no longer represented in parliament
\n\n
Aliancia Nového Občana (Alliance of the New Citizen, ANO) was a populist right-wing party
founded in 2001 by media entrepreneur Pavol Rusko. The party was anti-establishment and
economically liberal. The party scored 8.0% of the vote in 2002 and was asked to join the
government coalition led by Mikuláš Dzurinda and his conservative Slovak Democratic and Christian
Union (SDKÚ). ANO delegates were appointed to Ministers of Culture, Health, and Industry, but
disagreements within the government coalition led to the expulsion of ANO from the cabinet in
2005. ANO did not re-enter parliament in 2006 and eventually dissolved in 2011.
\n\n
The Hnutie za Demokratické Slovensko (Movement for a Democratic Slovakia, HZDS; after 2000
named People’s Party – Movement for a Democratic Slovakia, ĽS-HZDS) was a populist party
founded in 1991 by Vladimír Mečiar as a split from Public Against Violence. The party was
nationalist, economically statist and protectionist, and Eurosceptic. The HZDS’ support peaked at
34.9% of the vote in the 1994 election (in alliance with the Peasant’s Party of Slovakia) and formed a
government with the far-left Union of the Workers of Slovakia and the far-right Slovak National 
Party. The party was marred by internal splits and defections, but managed to rank first with 19.5%
of the vote in 2002, when it was however relegated in opposition. The ĽS-HZDS last entered
parliament in 2006 (8.8% of the vote), when it was asked to join the Fico II government (2006-2010)
alongside Smer (Direction) and the far-right Slovak National Party. The ĽS-HZDS dissolved in 2014.
\n\n
The Komunistická Strana Slovenska (Communist Party of Slovakia, KSS) is a far-left party founded in
1992 as the merger of the Communist Party of Slovakia – 91 and the Communist League of Slovakia –
both non-reformist successors of the original Communist Party of Slovakia and the Communist Party
of Czechoslovakia. The party is anti-capitalist, anti-NATO, and Eurosceptic. The KSS reached 6.3% of
the vote in 2002, when it managed parliamentary representation for the first and last time.
The Pravá Slovenská Národná Strana (Real Slovak National Party, PSNS) was a populist far-right
party founded in 2001 by Ján Slota as an offshoot of the Slovak National Party (SNS). Just like the
SNS, the party was anti-minority, socially conservative, and Eurosceptic. The PSNS emerged as a
result of internal conflicts between then-chairwoman Anna Malíková and former chairman Slota
within SNS, which led to the expulsion of the latter alongside other dissident MPs. The PSNS
returned 3.7% of the vote in the 2002 election and merged back into the SNS in 2005, with Slota as
chairman.
\n\n
The Strana Občianskeho Porozumenia (Party of Civic Understanding, SOP) was a populist party
founded in 1998 by Košice mayor Rudolf Schuster, who was also president of Slovakia between 1999
and 2004. The party was economically liberal and pro-EU. The SOP gained 8.0% of the vote in the
1998 election and was asked to join the government coalition led by Mikuláš Dzurinda and his Slovak
Democratic Coalition (later Slovak Democratic and Christian Union). The party dissolved in 2003 and
most of its members joined Smer (Direction).
\n\n
The Slovenská Národná Strana (Slovak National Party, SNS) is a populist far-right party founded in
1989 that claims links to the historical party of the same name. The party is anti-minority, antiimmigration, socially conservative, pro-market, and Eurosceptic. Internal disputes within the party
led to an internal split in 2001, leading to the concomitant existence of the SNS and the Real Slovak
National Party. The two parties merged back together into the SNS in 2005 and the party scored its
best performance to date in 2006 (11.7% of the vote). Under the leadership of Ján Slota, the SNS
took part in two government coalitions, in 1994-1998 (HZDS-led Mečiar I government) and 2006-
2010 (Smer-led Fico I government). The SNS was embroiled in cases of corruption during its term as
junior coalition partner of the Fico I government and failed to cross the threshold for representation
in 2012. In 2012, Andrej Danko was appointed chairman and tried to steer the party in a more
moderate direction. Former chairman Ján Slota was expelled from the party in 2013. The SNS
returned to parliament in 2016 (8.6% of the vote) and took part in the Fico III government with
Smer, the ethno-liberal Most-Híd (Bridge), and the right-wing Sieť (Network). In 2020, the party
failed to re-enter parliament scoring 3.2% of the vote.
\n\n
Vlast’ (Homeland) is a populist far-right party active under this name since 2019. The party is antiimmigration, socially conservative, anti-LGBTQI+, Russophile, and Eurosceptic. Vlasť is led by former
judge and Minister of Justice Štefan Harabin and scored 2.9% of the vote in the 2020 election.
The Združenie Robotníkov Slovenska (Union of the Workers of Slovakia, ZRS) was a populist far-left
party founded in 1994 as a split from the Party of the Democratic Left. The party was anti-capitalist,
statist, and Eurosceptic. The ZRS gained 7.3% of the vote in the 1994 election and joined the Mečiar I
government (1994-1998), but failed to secure parliamentary representation thereafter. The party
dissolved in 2017.
  ",
# Slovenia ----
  "
### Parties represented in parliament
\n\n
Levica (Left) is a populist far-left party established in 2017 as a successor of the United Left coalition,
active between 2014 and 2017. The party stands for ecological and democratic socialism,
sustainability, democratic economy planning, workplace democracy, and adopts an anti-NATO and
Eurosceptic stance. Between 2018 and 2019, Levica provided external support to the government 
coalition led by PM Marjan Šarec. The party gained 4.4% of the vote in the 2022 election and then
formed a coalition government with the green-liberal Svoboda! (Freedom!) and the Social
Democrats.
\n\n
Nova Slovenija – Krščanski Demokrati (New Slovenia – Christian Democrats, N.Si) party is a
populist far-right party founded in 2000 as a breakaway from the Slovenian People’s Party and
Slovene Christian Democrats (SLS+SKD). The party started out as a traditional Christian conservative
party but progressively turned into a populist (since 2011) and far-right (since 2015) force. The N.Si is
currently nationalist, anti-immigrant, anti-LGBTQI+, socially and fiscally conservative as well as probusiness and economically neoliberal. The party has joined each of the three Janša-led governments
and supported crackdowns on liberal civil society, subscribed to cronyism and rent-seeking, and
endorsed institutions managed by the Slovenian Catholic Church. In the 2022 election, N.Si obtained
6.9% of the vote.
\n\n
The Slovenska Demokratska Stranka (Slovenian Democratic Party, SDS) is a populist far-right party
founded in 1989 and continuously led by Janez Janša since 1993. The SDS started out as a moderate
force but progressively turned into a populist (since 2000) and far-right (since 2015) party. The SDS is
currently nationalist, anti-immigrant, socially conservative, and economically liberal, with a generic
pro-EU outlook. The SDS has been embroiled in several scandals over the years, including a
conviction for bribery for Janša in 2013. The party bears a very strained relationship with the
(independent) media and has supported crackdowns on liberal civil society. The party came first in
the 2018 general election (24.9% of the vote) but was able to form a government (2020-2022) only
after the collapse of the coalition led by the List of Marjan Šarec. In the 2022 election, the SDS lost to
the green-liberal Svoboda! (Freedom!), gaining 23.5% of the vote.
\n\n
### Parties not/no longer represented in parliament
\n\n
Povežimo Slovenijo (Let’s Connect Slovenia, PoS) is a borderline populist right-wing coalition
including five established and newly formed conservative political parties active in rural areas of
Slovenia, i.e. the Slovenian People’s Party (SLS), Concretely (Konkretno), New People’s Party of
Slovenia (NLS), New Social Democracy (NSD), and Green Slovenia (ZS). The official agreement
between the parties was signed in February 2022. PoS managed 3.4% of the vote in the 2022
election, failing to cross the threshold.
\n\n
Resni.ca (Truth) is a populist party founded in 2021 by Zoran Stevanović. The party emerged in
reaction to COVID-19 restrictions and has a pronounced conspiracist outlook. The party is
ideologically ambiguous and presents anti-corruption, nationalist, and Eurosceptic elements. The
party contested the 2022 election returning 2.9% of votes and no MPs.
Slovenija Je Naša (Slovenia Is Ours, SJN) was a marginal populist party founded in 2004 by four-time
mayor of Koper, Boris Popovič. The party was regionalist and pro-business. SJN peaked at 2.6% of
the vote in the 2004 election and never managed parliamentary representation.
\n\n
The Slovenska Nacionalna Stranka (Slovenian National Party, SNS) is a populist far-right party
founded in 1991 by Zmago Jelinčič Plemeniti. The party is anti-immigrant, anti-NATO, and hard
Eurosceptic. Although liberal on issues such as abortion and religion, the SNS firmly opposes samesex marriage and is anti-Islam. The party peaked at 10.0% of the vote in the 1992 election.
The Socialistična Stranka Slovenije (Socialist Party of Slovenia, SSS) was a far-left party founded in
1990 as the successor of the Socialist Union of Working People of Slovenia. The party was 
democratic socialist and supported Slovenian independence from Yugoslavia. The SSS scored 5.4% of
the vote in the 1990 election but failed to re-enter parliament in 1992. The party merged into Liberal
Democracy of Slovenia in 1994.
\n\n
Lista Marjana Šarca (List of Marjan Šarec, LMŠ) is a former populist party (until 2018) founded in
2014 by the homonymous former comedian and Kamnik mayor Marjan Šarec. The LMŠ is an antiestablishment and anti-corruption social-liberal force advocating reform in different areas (e.g.
electoral system, public administration, IT, environment, etc.). Šarec ranked second in the 2017
presidential election and scored 12.6% of the vote in the 2018 general election. The party largely
dropped its populist profile upon forming a short-lived minority government (2018-2020) with the
Social Democrats, the Modern Centre Party, the Party of Alenka Bartušek, and the Democratic Party
of Pensioners, listing Šarec as PM. The party scored 3.7% of the vote in the 2022 election and thus
lost parliamentary representation.
\n\n
Združena Levica (United Left, ZL) was a populist far-left coalition founded in 2014 and including the
Democratic Labour Party, Party for Sustainable Development of Slovenia, and Initiative for
Democratic Socialism. The coalition had a populist anti-establishment profile and sought to present a
democratic socialist alternative to the neoliberal status quo. ZL was Eurosceptic and advocated
thorough reform of the EU. The coalition dissolved in 2017, when it was succeeded by Levica.
  ",
# Spain ----
  "
### Parties represented in parliament
\n\n
There are several regional far left electoral coalitions and parties that currently hold seats in
parliament (BNG, EHB and CUP). As these parties are all very small we will not discuss them in detail
here.
\n\n
In 2014, Pablo Iglesias and other academics founded Podemos (We Can). Podemos is the political
product of the anti-austerity Indignados Movement – a series of protests against the austerity
measures during the Great Recession. The party initially combined a strongly populist outlook with a
far-left ideology (and was inspired by Latin American populists like Hugo Chávez). In 2016, it formed
an electoral alliance with several other far-left parties (Unidas Podemos, United We Can). In 2019
Podemos joined a government coalition with the social democrats. Because Podemos strongly
moderated its populism during the 2019 election campaign, we have decided to include the party as
not populist anymore from 2019 onwards.
\n\n
For a long time Spain was a country without an influential far right party. Yet, in 2013, Vox was
founded. In the first few years of its existence, the party was electorally unsuccessful. Yet, after the
elections in 2019, the party entered the national parliament with 15 percent of the vote. Vox is a farright and populist party, but it is also explicitly anti-feminist and right-wing in economic terms. The
party attacks what it calls the “progressive dictatorship” and can also be categorised as Eurosceptic.
\n\n
### Parties not/no longer represented in parliament
\n\n
Herri Batasuna (Popular Unity, HB) is a regional far-left party that has no parliamentary seats
anymore. As this party is both regional and very small, we do not discuss it in detail here.
One of the far-left parties that is now part of the Unidas Podemos coalition that deserves
mentioning is Izquierda Unida (United Left, IU). IU was founded in 1986 – being itself also a coalition
of several other parties and organisations, the most important of which is the Communist Party of
Spain (PCE). IU is presented as a historical case. It is still represented in parliament, but it is part of 
the Unidas Podemos alliance, and as such is not categorised as a separate party since joining Unidas
Podemos in 2016.
  ",
# Sweden ----
  "
### Parties represented in parliament
\n\n
Sverigedemokraterna (Sweden Democrats, SD) was founded in 1988. Nowadays the party is a
radical right party, but it has its roots in Swedish fascism. The SD started a process of moderation in
the mid-1990s. In 2001, the most extreme faction was expelled and from then onwards the party
also explicitly denounced Nazism. We therefore classify the party as far-right since its inception and
populist since 2001. In 2005, Jimmie Åkesson was elected party leader and he continued the process
of moderation. The party entered parliament for the first time in 2010. Until the end of the 2010s,
mainstream parties established a cordon sanitaire because of the fascist past of the party. In the
2022 elections, the party increased its vote share to 20.5% and became the second largest party.
\n\n
Vänsterpartiet (Left Party, V) is the successor to the Communist Party of Sweden. It was founded in
1917 and changed its name to Vänsterpartiet in 1990. The party is strongly left-wing and moderately
Eurosceptic. Yet, the party is also progressive when it comes to, for example, the environment and
feminism. Because of its relatively moderate outlook, we classify the party as borderline far left. V
provided parliamentary support for several cabinets led by the social democrats. The party leader is
Mehrnoosh \"Nooshi\" Dadgostar.
\n\n
### Parties not/no longer represented in parliament
\n\n
The populist party Ny Demokrati (New Democracy, NyD) was founded in 1991 and dissolved in
2000. Although the party was strict on immigration, it more strongly emphasised socioeconomic
issues. Because the party did not focus primarily on immigration, we do not classify it as far right.
Interestingly, the party supported EU membership. 
  ",
# Switzerland ----
  "
### Parties represented in parliament
\n\n
The Eidgenössisch-Demokratische Union (Federal Democratic Union, EDU), founded in 1975, is a
fundamentalist protestant far-right party. EDU has strong culturally-conservative as well as nativist
positions. As the Christian Bible is its main ideological reference (‘the will of God’ instead of the ‘will
of the people’), it cannot genuinely be considered populist. The party also lacks a clear anti-elitist
discourse. Typically polling around 1% of the vote, it has gained representation in federal parliament
for most of the years since 1991.
\n\n
The regionalist populist Lega dei Ticinesi (Ticino League, LdT), which was founded in 1991, has been
represented in the federal parliament (National Council) with one or two seats throughout its
existence. This is a far-right party marked by its anti-immigration and Eurosceptic stance. Its populist
anti-establishment discourse is marked by calls to defend the interest of residents in the Ticino
canton, while LdT also seeks to defend Swiss sovereignty against EU influence.
\n\n
The communist Partei der Arbeit der Schweiz (Swiss Party of Labour, PdA) has been represented
with seats in the National Council for most of the years since 1947, although its support has
dwindled since the 1970s. Holding on to traditional socialist and Marxist principles, it is not a clear
exponent of populism. In the 2019 federal election, the PdA competed in cantonal alliances with the
ideologically close solidaritéS (SOL, Solidarity), which itself held one seat previously between 1999
and 2007. While both parties have affiliations with like-minded organisations in other European
countries, they are marked by typical left-wing critiques of the EU. Both parties have been classified
as far left.
\n\n
The Schweizerische Volkspartei (Swiss People’s Party, SVP) has been part of the consociationalist
federal government since its foundation in 1971 (with only a short interruption in 2008). The party
has received consistent support since the late 1990s and has been the largest party in Switzerland
since 2003. Since 1999, the SVP has received between 22.5% and 29.4% of the Swiss-wide vote, and
it has been the largest party in Switzerland since 2003. Its electoral growth in the 1990s was
accompanied by an ideological radicalisation of the formerly agrarian-conservative party. The Zurich
branch, led by Christoph Blocher, became dominant at the federal level, and steered the party in a
more populist, xenophobic and Eurosceptic direction. Under Blocher, the party began to criticise the 
‘political class’, and called for anti-immigration measures and the preservation of Swiss sovereignty.
It is therefore classified as populist and far right.
\n\n
### Parties not/no longer represented in parliament
\n\n
Two parties that did combine a far-right ideology with populism have by now disappeared from the
National Council. In the election of 2011, the Mouvement citoyens genevois (Geneva Citizens'
Movement, MCG) managed to win one seat in the National Council, opposing the political
establishment, promoting a leaner state and supporting small and medium enterprise. The MCG has
consistently opposed cross-border labour migration from France, illustrating its nativist character.
Still represented at the cantonal level, it lost representation on the National Council in 2019.
\n\n
Since its foundation in the 1960s, the Schweizer Demokraten (Swiss Democrats, SD) has presented
itself as a political outsider, defending the Swiss people against immigration. SD long occupied a
handful of seats prior to their gradual decline and disappearance from parliament in 2007. 
  ",
# United Kingdom ----
  "
### Parties represented in parliament
\n\n
The Democratic Unionist Party (DUP), founded in 1971, is a party in Northern Ireland in favor of British
unionism. The party is rather conservative and Eurosceptic and has supported a minority cabinet led by
the Conservatives between 2017 and 2019. The DUP is a borderline populist party, as it occasionally uses
populist tropes.
\n\n
### Parties not/no longer represented in parliament
\n\n
The United Kingdom Independence Party (UKIP) was founded in 1993 as the successor of the AntiFederalist League. Initially a single-issue party advocating EU exit, it gradually evolved to a fully-fledged
populist and far-right party. Its performance peaked under the leadership of Nigel Farage (2006-2009 and
2010-2016) under the single-issue banner of EU withdrawal. Farage resigned from the party’s leadership
in 2016 in the aftermath of Brexit, finally departing from the party in 2018. Since then, the party lost many
voters. This resulted in a programmatic shift to the far right grassroots sector, and a change in almost all
the party’s positions.
\n\n
The Brexit Party (now Reform) was established in 2018 by Nigel Farage with a clear plan to deliver Brexit
during a time when the UK mainstream parties were encountering difficulties in sealing a deal with the
EU. It is a far-right populist party e. The party was renamed Reform in 2021, broadening its agenda to 
focus on anti-lockdown policies. The Brexit Party gained the largest share of votes in the 2019 EP election
(30.5%) in sharp contrast to UKIP’s 3.2%. However, following a change in Conservative Party leadership
and Johnson’s subsequent promise to deliver a deal, the 2019 UK General election produced a strong
result for the Conservative Party at the expense of the Brexit Party which has since broadened its agenda.
Respect (R) was founded in 2004, as a populist, far left and Eurosceptic party. It emerged in the aftermath
of the 2003 Iraq war, and consisted of antiglobalisation activists and anti-war protesters. What made this
party distinctive from other far left parties is the merger of revolutionary international socialism and antiZionism. Respect also had links to the Muslim Association of Britain. It was dissolved in 2016. 
  "
)


longitudes <- c(
  14.5501,  # Austria
  4.4699,   # Belgium
  25.4858,  # Bulgaria
  15.2000,  # Croatia
  33.4299,  # Cyprus
  15.4730,  # Czech Republic
  9.5018,   # Denmark
  25.0136,  # Estonia
  25.7482,  # Finland
  2.2137,   # France
  10.4515,  # Germany
  21.8243,  # Greece
  19.5033,  # Hungary
  -19.0208, # Iceland
  -8.2439,  # Ireland
  12.5674,  # Italy
  24.6032,  # Latvia
  23.8813,  # Lithuania
  6.1296,   # Luxembourg
  5.2913,   # Netherlands
  8.4689,   # Norway
  19.1451,  # Poland
  -8.2245,  # Portugal
  24.9668,  # Romania
  19.6990,  # Slovakia
  14.9955,  # Slovenia
  -3.7492,  # Spain
  18.6435,  # Sweden
  8.2275,   # Switzerland
  -3.4360   # United Kingdom
)



latitudes <- c(
  47.5162, # Austria
  50.5039, # Belgium
  42.7339, # Bulgaria
  45.1000, # Croatia
  35.1264, # Cyprus
  49.8175, # Czech Republic
  56.2639, # Denmark
  58.5953, # Estonia
  61.9241, # Finland
  46.2276, # France
  51.1657, # Germany
  39.0742, # Greece
  47.1625, # Hungary
  64.9631, # Iceland
  53.4129, # Ireland
  41.8719, # Italy
  56.8796, # Latvia
  55.1694, # Lithuania
  49.8153, # Luxembourg
  52.1326, # Netherlands
  60.4720, # Norway
  51.9194, # Poland
  39.3999, # Portugal
  45.9432, # Romania
  48.6690, # Slovakia
  46.1512, # Slovenia
  40.4637, # Spain
  60.1282, # Sweden
  46.8182, # Switzerland
  55.3781  # United Kingdom
)

iso_codes <- c(
  "AUT", # Austria
  "BEL", # Belgium
  "BGR", # Bulgaria
  "HRV", # Croatia
  "CYP", # Cyprus
  "CZE", # Czech Republic
  "DNK", # Denmark
  "EST", # Estonia
  "FIN", # Finland
  "FRA", # France
  "DEU", # Germany
  "GRC", # Greece
  "HUN", # Hungary
  "ISL", # Iceland
  "IRL", # Ireland
  "ITA", # Italy
  "LVA", # Latvia
  "LTU", # Lithuania
  "LUX", # Luxembourg
  "NLD", # Netherlands
  "NOR", # Norway
  "POL", # Poland
  "PRT", # Portugal
  "ROU", # Romania
  "SVK", # Slovakia
  "SVN", # Slovenia
  "ESP", # Spain
  "SWE", # Sweden
  "CHE", # Switzerland
  "GBR"  # United Kingdom
)


reports <- tibble(
  input = "/Users/lukefischer/Dropbox/The PopuList Repo/Countries/example-report.qmd", 
  output_file = str_glue("/Reports/{countries}.html"),
  execute_params = pmap(
    list(countries, texts, longitudes, latitudes, iso_codes), 
    ~list(
      country = ..1, 
      text = ..2, 
      longitude = ..3, 
      latitude = ..4,
      iso_code = ..5
    )
  )
)

pwalk(reports, quarto_render)
