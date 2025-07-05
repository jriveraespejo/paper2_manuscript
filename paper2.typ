// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let target = query(it.target, loc).first()
  if it.at("supplement", default: none) == none {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}
#show: doc => article(
  title: [Let’s talk about Thurstone & Co.: An information-theoretical model for comparative judgments, and its statistical translation],
  authors: (
    ( name: [Jose Manuel Rivera Espejo],
      affiliation: [University of Antwerp],
      email: [JoseManuel.RiveraEspejo\@uantwerpen.be] ),
    ( name: [Tine van Daal],
      affiliation: [University of Antwerp],
      email: [tine.vandaal\@uantwerpen.be] ),
    ( name: [Sven De Maeyer],
      affiliation: [University of Antwerp],
      email: [sven.demaeyer\@uantwerpen.be] ),
    ( name: [Steven Gillis],
      affiliation: [University of Antwerp],
      email: [steven.gillis\@uantwerpen.be] ),
    ),
  date: [2025-07-04],
  abstract: [This study revisits Thurstone’s law of comparative judgment \(CJ) by addressing two central issues in the CJ literature. First, it critiques the widespread reliance on Thurstone’s Case V assumptions and, by extension, the Bradley-Terry-Luce \(BTL) model in the analysis of CJ data. Specifically, this study argues that while the assumptions of equal discriminal dispersions and zero correlation between stimuli simplify the trait measurement model, they may fail to capture the complexity of some traits or account for heterogeneous stimuli, potentially leading to unreliable and inaccurate trait estimates. Second, the study highlights the apparent disconnect between trait measurement and hypothesis testing procedures in CJ applications. It contends that while separating these procedures simplifies the analysis of CJ data, this practice can also undermine the resulting statistical inferences.

To address these issues, this study aims to extend Thurstone’s general form using a systematic and integrated approach based on causal and Bayesian inference methods. This extension integrates Thurstone’s core theoretical principles alongside key CJ assessment design features. It then translates these elements into a probabilistic statistical model for analyzing dichotomous CJ data, overcoming the rigid assumptions of Case V and the BTL model.

Finally, the study emphasizes the relevance of this extension for contemporary empirical CJ research, particularly stressing the need for bespoke CJ models tailored to the experiments and data assumptions. It also lays the foundation for broader applications, encouraging researchers across the social sciences to adopt more robust and interpretable methodologies.

],
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)


= Introduction
<sec-introduction>
In #emph[comparative judgment] \(CJ) studies, judges assess a specific trait or attribute across different stimuli by performing pairwise comparisons #cite(<Thurstone_1927a>);#cite(<Thurstone_1927b>);. Each comparison produces a dichotomous outcome, indicating which stimulus is perceived to have a higher trait level. For example, judges can compare pairs of written texts \(the stimuli) to determine the relative writing quality each text exhibit \(the trait) #cite(<Pollitt_2012b>);#cite(<vanDaal_et_al_2016>);#cite(<Lesterhuis_2018_thesis>);#cite(<Coertjens_et_al_2017>);#cite(<Goossens_et_al_2018>);#cite(<Bouwer_et_al_2023>);.

Numerous studies have documented the effectiveness of CJ in assessing traits and competencies over the past decade. These studies have highlighted three key strengths of the method: its reliability, its validity, and practical applicability. Research on reliability suggests that CJ requires a relatively modest number of pairwise comparisons #cite(<Verhavert_et_al_2019>);#cite(<Crompvoets_et_al_2022>) to generate trait scores that are as precise and consistent as those generated by other assessment methods #cite(<Coertjens_et_al_2017>);#cite(<Goossens_et_al_2018>);#cite(<Bouwer_et_al_2023>);. In addition, the evidence suggests that the reliability and time efficiency of CJ are comparable, if not superior, to those of other assessment methods when employing adaptive comparison algorithms #cite(<Pollitt_2012b>);#cite(<Verhavert_et_al_2022>);#cite(<Mikhailiuk_et_al_2021>);. Meanwhile, research on validity indicates the capacity of CJ scores to represent accurately the traits under measurement #cite(<Whitehouse_2012>);#cite(<vanDaal_et_al_2016>);#cite(<Lesterhuis_2018_thesis>);#cite(<Bartholomew_et_al_2018>);#cite(<Bouwer_et_al_2023>);. Lastly, research on its practical applicability highlights CJ’s versatility across educational and non-educational contexts #cite(<Kimbell_2012>);#cite(<Jones_et_al_2015>);#cite(<Bartholomew_et_al_2018>);#cite(<Jones_et_al_2019>);#cite(<Marshall_et_al_2020>);#cite(<Bartholomew_et_al_2020b>);#cite(<Boonen_et_al_2020>);.

Nevertheless, despite the increasing number of CJ studies, research in this domain remains unsystematic and fragmented, leaving several critical issues unresolved. This study identifies and discusses two prominent issues in the CJ literature that can undermine the reliability and validity of CJ’s trait estimates. First, it critiques the widespread reliance on Thurstone’s Case V assumptions #cite(<Thurstone_1927b>) and, by extension, the Bradley-Terry-Luce \(BTL) model #cite(<Bradley_et_al_1952>);#cite(<Luce_1959>) in the analysis of CJ data. Specifically, this study argues that while the assumptions of equal discriminal dispersions and zero correlation between stimuli simplify the trait measurement model, they may fail to capture the complexity of some traits or account for heterogeneous stimuli, potentially leading to unreliable and inaccurate trait estimates. Second, the study highlights the disconnect between trait measurement and hypothesis testing procedures in CJ applications. In particular, it contends that while separating these procedures simplifies the analysis of CJ data, this practice can also undermine the resulting statistical inferences #cite(<McElreath_2020>);#cite(<Kline_et_al_2023>);#cite(<Hoyle_et_al_2023>);.

To address these issues, this study aims to extend Thurstone’s general form through a systematic and integrated approach that combines causal and Bayesian inference methods. This extension integrates Thurstone’s core theoretical principles alongside key CJ assessment design features, such as the selection of judges, stimuli, and comparisons. In addition to potentially enhancing measurement reliability and validity, and improving statistical accuracy in hypothesis testing, this approach offers two key advantages. First, it clarifies the interactions among all actors and processes involved in CJ assessments. Second, it shifts the current comparative data analysis paradigm from passively accepting Case V and the BTL model assumptions to actively testing whether those assumptions fit the data under analysis.

The remainder of this study is organized into seven sections. @sec-thurstone_theory provides an overview of Thurstone’s theory. @sec-theory-issues examines the central issues identified in the CJ literature. @sec-theoretical extends Thurstone’s general form to address these issues. The extension integrates core theoretical principles alongside key CJ assessment design features, such as the selection of judges, stimuli, and comparisons. @sec-statistical translates these theoretical and design elements into a probabilistic statistical model to analyze dichotomous pairwise comparison data. @sec-discussion reviews the findings, outline directions for future research, discusses the study’s limitations and details the challenges that applied researchers may encounter. Finally, @sec-conclusion provides the concluding remarks.

= Thurstone’s theory
<sec-thurstone_theory>
In its #emph[general form];, Thurstone’s theory addresses pairwise comparisons of a single judge who evaluates multiple stimuli #cite(<Thurstone_1927a>);#cite(<Thurstone_1927b>);. The theory posits that two key factors determine the dichotomous outcome of these comparisons: the discriminal process of each stimulus and their discriminal difference. The #emph[discriminal process] captures the psychological impact each stimulus exerts on the judge or, more simply, his perception of the stimulus trait. The theory assumes that the discriminal process for any given stimulus forms a Normal distribution along the trait continuum #cite(<Thurstone_1927b>);. The mode \(mean) of this distribution, known as the #emph[modal discriminal process];, indicates the stimulus position on this continuum, while its dispersion, referred to as the #emph[discriminal dispersion];, reflects variability in the perceived trait of the stimulus.

These ideas become clearer with an example. @fig-discriminal_process[45127368-afa1-446a-820f-fc64c546b2c5%fig-thurstone_theory] illustrates the hypothetical discriminal processes for two written texts along a #emph[quality] trait continuum. The figure shows that the modal discriminal process for Text B lies further along the continuum than that of Text A $(T_B > T_A)$, suggesting that Text B exhibits higher quality. The figure also shows that Text B has a broader distribution than Text A, indicating a greater variability in perception due to its larger discriminal dispersion $(sigma_B > sigma_A)$.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
#box(width: 100%,image("./images/png/discriminal_process.png"))
], caption: figure.caption(
position: bottom, 
[
Hypothetical discriminal processes for two written texts
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-discriminal_process>


]
,
  [
#figure([
#box(width: 100%,image("./images/png/discriminal_difference.png"))
], caption: figure.caption(
position: bottom, 
[
Hypothetical discriminal difference of two written texts
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-discriminal_difference>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Hypothetical discriminal processes and discriminant difference along a quality trait continuum for two written texts.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-thurstone_theory>


However, since the individual discriminal processes of the stimuli are not directly observable #cite(<Thurstone_1927a>);, the theory introduces the #emph[law of comparative judgment];. This law posits that in pairwise comparisons, a judge perceives the stimulus with a discriminal process positioned further along the trait continuum as possessing more of the trait #cite(<Bramley_2008>);. This suggests that pairwise comparison outcomes depend on the relative distance between stimuli, not their absolute positions on the continuum. Indeed, the theory assumes that the difference between the underlying discriminal processes of the stimuli, referred to as the #emph[discriminal difference];, determines the observed dichotomous outcome. Furthermore, the theory assumes that because the individual discriminal processes form a Normal distribution on the continuum, the discriminal difference will also conform to a Normal distribution #cite(<Andrich_1978>);. In this distribution, the mode \(mean) represents the average relative separation between the stimuli $(T_B - T_A)$, and its dispersion $sigma_(B A)$ indicates the variability of that separation.

@fig-discriminal_difference[45127368-afa1-446a-820f-fc64c546b2c5%fig-thurstone_theory] illustrates the distribution of the discriminal difference for the two hypothetical texts. The figure indicates that the judge perceives Text B as having significantly higher quality than Text A. Two key observations support this conclusion: the positive difference between their modal discriminal processes $(T_B - T_A > 0)$ and the probability area where the discriminal difference distinctly favors Text B over Text A, represented by the shaded gray area denoted as $P (B > A)$. As a result, the dichotomous outcome of this comparison is more likely to favor Text B over A.

= Two Prominent Issues in Traditional CJ Practice
<sec-theory-issues>
Thurstone noted from the outset that his general formulation, described in @sec-thurstone_theory, led to a #emph[trait scaling problem];. Specifically, the model required estimating more "unknown" parameters than the number of available pairwise comparisons #cite(<Thurstone_1927b>);. For instance, in a CJ assessment with five texts, the general form would require estimating $20$ parameters: five modal discriminal processes, five discriminal dispersions, and $10$ correlations–one per comparison. However, a single judge could only provide $binom(5, 2) = 10$ unique comparisons, an "insufficient" data set to estimate the required parameters.

To address this issue and facilitate the practical implementation of the theory, Thurstone developed five cases derived from this general form, each progressively incorporating additional simplifying assumptions #cite(<Thurstone_1927b>);. In Case I, Thurstone postulated that pairs of stimuli would maintain a constant correlation across all comparisons. In Case II, he allowed multiple judges to undertake comparisons instead of confining evaluations to a single judge. In Case III, he posited that there was no correlation between stimuli. In Case IV, he assumed that the stimuli exhibited similar dispersions. Finally, in Case V, he replaced this assumption with the condition that stimuli had equal discriminal dispersions. @tbl-thurstone_cases summarizes the assumptions of the general form and the five cases. For a detailed discussion of these cases and their growing simplification, refer to #cite(<Thurstone_1927b>) and #cite(<Bramley_2008>);.

#figure([
#box(width: 100%,image("./images/png/thurstone_cases.png"))
], caption: figure.caption(
position: top, 
[
Thurstones cases and their asumptions
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
numbering: "1", 
)
<tbl-thurstone_cases>


As the table suggest, Thurstone developed Case V with an emphasis on statistical simplicity, but this simplicity comes at the expense of accurate and precise trait measurement and practical guidance for inference. Specifically, Thurstone cautioned that Case V use "should not be made without \(an) experimental test" #cite(<Thurstone_1927b>)[, p.~270];, as it imposes the most extensive set of simplifying assumptions #cite(<Bramley_2008>);#cite(<Kelly_et_al_2022>) \(see @tbl-thurstone_cases). Moreover, because Thurstone’s primary goal was to produce a "rather coarse scaling" of traits and "allocate the compared stimuli on this continuum" #cite(<Thurstone_1927b>)[, p.~269];, his theory did not support formal statistical inference. Nevertheless, despite Case V’s limitations, CJ research has predominantly relied on it to measure various traits, raising significant concerns about the reliability and validity of such measurements in contexts where its assumptions may not hold #cite(<Kelly_et_al_2022>);#cite(<Andrich_1978>);. Furthermore, although the CJ tradition has attempted to address the gap in statistical inference by relying on the point estimates of traits–or their transformations–the statistical literature cautions against using these estimates as the sole basis for inference, as such practices introduce bias and reduce the precision of hypothesis tests #cite(<McElreath_2020>);#cite(<Kline_et_al_2023>);#cite(<Hoyle_et_al_2023>);. The next sections discuss both issues in greater depth.

== The Case V and the statistical analysis of CJ data
<sec-theory-issue1>
Case V is the most widely used model in the CJ literature. This preference largely stems from the widespread adoption of the BTL model, which provides a simplified statistical representation of the case. The BTL model incorporates most of Case V’s assumptions, with one notable exception. While Case V assumes a Normal distribution for the stimuli’ discriminal processes, the BTL model uses the more mathematically tractable Logistic distribution #cite(<Andrich_1978>);#cite(<Bramley_2008>) \(see @tbl-thurstone_cases). However, this substitution has minimal impact on trait estimation or model interpretation because the scale of the discriminal process \(i.e., the latent trait) is arbitrary up to a non-monotonic transformation #cite(<vanderLinden_et_al_2017_I>);#cite(<McElreath_2021>);. In other words, as long as the substitution \(transformation) preserves the rank order of the data, the choice of distribution for the discriminal processes is inconsequential. The BTL model satisfies this condition because the Normal and Logistic distributions exhibit analogous statistical properties, differing only by a scaling factor of approximately $1.7$ #cite(<vanderLinden_et_al_2017_I>);.

However, Thurstone acknowledged that some assumptions of Case V are problematic when assessing complex traits or heterogeneous stimuli #cite(<Thurstone_1927a>);. Thus, given that contemporary CJ applications often involve such traits and stimuli, two key assumptions of Case V, and by extension, the BTL model, may not always hold in theory or practice. These assumptions are the equal dispersion and zero correlation between stimuli.

=== The assumption of equal dispersions between stimuli
<sec-theory-issue1a>
According to the theory, discrepancies in the discriminal dispersions of stimuli shape the distribution of the discriminal difference, directly influencing the outcome of pairwise comparisons. A thought experiment can help illustrate this idea. Suppose researchers observe the discriminal processes for two texts, A and B, assuming that the dispersion for Text A remains constant and that the two texts are uncorrelated $(rho = 0)$. @fig-dispersion[45127368-afa1-446a-820f-fc64c546b2c5%fig-casev_issues] demonstrates that an increase in the uncertainty associated with the perception of Text B relative to Text A $(sigma_B - sigma_A)$, broadens the distribution of their discriminal difference. This broadening affects the probability area where the discriminal difference distinctly favors Text B over Text A, expressed as $P (B > A)$, ultimately influencing the comparison outcome. Additionally, the figure reveals that when the discriminal dispersions of the texts are equal, as in the BTL model $(sigma_B - sigma_A = 0)$, the discriminal difference distribution is more narrow than when the dispersions differ. As a result, the discriminal difference is more likely to favor Text B over Text A, as it is represented by the shaded gray area.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
#box(width: 100%,image("./images/png/dispersion.png"))
], caption: figure.caption(
position: bottom, 
[
Discriminal Difference distribution under varying discrepancies in stimuli dispersions
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-dispersion>


]
,
  [
#figure([
#box(width: 100%,image("./images/png/correlation.png"))
], caption: figure.caption(
position: bottom, 
[
Discriminal Difference distribution under varying levels of correlation between stimuli
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-correlation>


]
)
]
], caption: figure.caption(
position: bottom, 
[
The effect of dispersion discrepancies and stimuli correlation on the distribution of the discriminal difference.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-casev_issues>


In experimental practice, however, the thought experiment occurs in reverse. Researchers first observe the comparison outcome and then use the BTL model to infer the discriminal difference between stimuli and their respective discriminal processes #cite(<Thurstone_1927a>);. Consequently, the effectiveness of the outcome to reflect #emph[true] differences between stimuli largely depends on the validity of the model’s assumptions #cite(<Kohler_et_al_2019>);, in this case, the assumption of equal dispersions. When the assumption accurately captures the complexity of the data, the BTL model estimates a discriminal difference distribution that accurately represents the #emph[true] discriminal difference between the texts. This scenario is illustrated in @fig-dispersion[45127368-afa1-446a-820f-fc64c546b2c5%fig-casev_issues], when the model’s discriminal difference distribution aligns with the #emph[true] discriminal difference distribution, represented by the thick continuous line corresponding to $sigma_B - sigma_A = 0$. The accuracy of this discriminal difference then ensures reliable estimates for the texts’ discriminal processes.

Notably, while assuming equal dispersions simplifies the trait measurement model, evidence from the CJ literature suggests that this assumption may not hold for heterogeneous stimuli, such as handwritten texts or English compositions #cite(<Thurstone_1927a>);#cite(<Andrich_1978>);#cite(<Bramley_2008>);#cite(<Kelly_et_al_2022>);. The presence of the so-called misfit texts signals this limitation. #emph[Misfit texts] are those that elicit more judgment discrepancies than others #cite(<Pollitt_2004>);#cite(<Pollitt_2012b>);#cite(<Pollitt_2012a>);#cite(<Goossens_et_al_2018>);, and these discrepancies may arise from larger discriminal dispersions caused by the stimulus’ heterogeneity or because the texts are genuine outliers–i.e., texts with distinctive characteristics that deviate markedly from the rest of the sample in which they occur #cite(<Grubbs_1969>);. In either case, the BTL model’s assumptions prevent it from adequately accounting for or addressing these anomalies, leaving exclusion of such "problematic" texts as the primary remedy #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);.

Significant statistical and measurement issues can arise when the assumption of equal dispersions between stimuli does not hold. Specifically, the BTL model may overestimate the trait’s reliability, that is, the degree to which the outcome accurately reflects the #emph[true] discriminal differences between stimuli. This overestimation, in turn, results in spurious conclusions about these differences #cite(<McElreath_2020>);#cite(<Wu_et_al_2022>) and, by extension, about the underlying discriminal processes of stimuli. @fig-dispersion[45127368-afa1-446a-820f-fc64c546b2c5%fig-casev_issues] also illustrates this scenario when the model’s discriminal difference distribution aligns with the thick continuous line for $sigma_B - sigma_A = 0$, while the #emph[true] discriminal difference follows any discontinuous line where $sigma_B - sigma_A eq.not 0$. Furthermore, if it is acknowledged that #emph[misfit statistics] may represent texts with different dispersions or outlying observations, the common CJ practice of excluding stimuli based on these statistics may unintentionally discard valuable information #cite(<Miller_2023>);, and introduce bias into the trait estimates #cite(<Zimmerman_1994>);#cite(<McElreath_2020>);. The direction and magnitude of these biases remain unpredictable, as they depend on which stimuli are excluded from the analysis.

=== The assumption of zero correlation between stimuli
<sec-theory-issue1b>
The correlation between two stimuli $rho$ measures how much the judges’ perception of a specific trait in one stimulus depends on their perception of the same trait in other stimulus. Similar to the discriminal dispersions, this correlation shapes the distribution of the discriminal difference, directly impacting the outcomes of pairwise comparisons. Assuming that the discriminal dispersions for a couple of texts remain constant, @fig-correlation[45127368-afa1-446a-820f-fc64c546b2c5%fig-casev_issues] shows that as the correlation between the two texts increases, the distribution of their discriminal difference becomes narrower. This narrowing, in turn, affects the probability that the discriminal difference distinctly favors Text B over Text A–denoted as $P (B > A)$–and thus directly influences the comparison outcome. Furthermore, the figure shows that when two texts are independent or uncorrelated, as assumed in the BTL model $(rho = 0)$, the distribution of their discriminal difference is less narrow than in scenarios where the texts are positively correlated. As a result, it becomes less likely for the comparison to favor Text B over Text A, as indicated by the larger shaded area.

Despite these notable differences in the distribution of the discriminal difference under various correlational assumptions, in practice, assessment designs often adopt the assumption of no correlation between stimuli based on Thurstone’s early theoretical justification #cite(<Thurstone_1927b>);. He argued that stimuli could be treated as uncorrelated because judges’ biases–arising from two opposing and equally weighted effects occurring during the pairwise comparisons–would cancel each other out. This idea was later formalized by #cite(<Andrich_1978>);, who provided a mathematical demonstration of this cancellation using the BTL model under the assumption of discriminal processes with additive biases. However, evidence from the CJ literature indicates that the assumption of zero correlation does not hold in practice in at least two cases: \(1) when intricate aspects of multidimensional, complex traits or heterogeneous stimuli influence judges’ perceptions, \(2) when additional hierarchical structures are relevant to the stimuli.

Regarding the first case, research on text quality assessments suggests that when judges evaluate complex, multidimensional traits or heterogeneous stimuli, they often rely on a variety of intricate stimulus characteristics to inform their judgments #cite(<vanDaal_et_al_2016>);#cite(<Lesterhuis_et_al_2018>);#cite(<Chambers_et_al_2022>);. Regardless of their relevance, these characteristics may not be equally weighted or consistently opposed across comparisons. As a result, they may exert a disproportionate influence on judges’ perceptions, generating biases that persist rather than cancel out. For example, this might occur when a judge assessing the argumentative quality of a text is disproportionately influenced by the clarity of the handwriting, thereby favoring neatly written texts even if their arguments are weaker. Moreover, because the discriminal process of stimuli becomes an observable outcome only through the judges’ perceptions, these biases could introduce dependencies between the stimuli #cite(<vanderLinden_et_al_2017_II>);. While direct evidence for this exact scenario is limited, existing studies document the presence of judge’s biases in CJ contexts #cite(<Pollitt_et_al_2003>);#cite(<vanDaal_et_al_2016>);#cite(<Bartholomew_et_al_2020a>);, reinforcing the argument that the factors influencing pairwise comparisons do not always cancel each other out.

In the second case, the shared context or inherent connections introduced by additional hierarchical structures may create dependencies between stimuli–a statistical phenomenon known as clustering #cite(<Everitt_et_al_2010>);. For instance, when the same individual produces multiple texts, those texts often share several features such as writing style or overall quality. Although some CJ studies acknowledge the presence of such hierarchical structures and account for them #cite(<Boonen_et_al_2020>);, the treatment of this additional source of dependence in other research has often been insufficient. For instance, when CJ data include multiple samples of stimuli from the same individuals, researchers frequently rely on \(averaged) point estimates of the BTL scores to conduct subsequent analyses and tests at the individual level #cite(<Bramley_et_al_2019>);#cite(<Bouwer_et_al_2023>);#cite(<vanDaal_et_al_2017>);#cite(<Jones_et_al_2019>);#cite(<Gijsen_et_al_2021>);.

Thus, erroneously assuming zero correlation between stimuli can also lead to significant statistical and measurement issues. In particular, neglecting judges’ biases or relevant hierarchical structures can create dimensional mismatches in the model, leading to the over- or underestimation of trait reliability #cite(<Ackerman_1989>);#cite(<Hoyle_et_al_2023>) and even introduce statistical biases #cite(<Wu_et_al_2022>);. These inaccuracies can result in spurious conclusions about the discriminal differences #cite(<McElreath_2020>) and, by extension, the underlying discriminal processes of the stimuli. One such spurious conclusion could be the incorrect classification of stimuli \(or judges) as #emph[misfits];. @fig-correlation[45127368-afa1-446a-820f-fc64c546b2c5%fig-casev_issues] illustrates how assuming zero correlation can undermine trait reliability: the discriminal difference distribution of the BTL scores follows the thick continuous line $(rho = 0)$, while the #emph[true] discriminal difference may correspond to any discontinuous line where $rho eq.not 0$.

Finally, similar to misfit stimuli, removing #emph[misfit judges] risks discarding valuable information and introducing bias into trait estimates #cite(<Miller_2023>);. The direction and magnitude of these biases remain unpredictable, as they depend on which judges are excluded from the analysis #cite(<Zimmerman_1994>);#cite(<OHagan_2018>);#cite(<McElreath_2020>);. #emph[Misfit] judges are those whose assessments deviate markedly from the shared consensus #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);#cite(<vanDaal_et_al_2016>);#cite(<Goossens_et_al_2018>);#cite(<Wu_et_al_2022>);, often appearing as outliers under the BTL model #cite(<Wu_et_al_2022>);.

== The disconnect between trait measurement and hypothesis testing
<sec-theory-issue2>
In CJ studies, the BTL model is typically used to measure traits and position the compared stimuli along a latent continuum #cite(<Thurstone_1927b>);. The CJ literature shows that studies frequently relies on point estimates of these traits–typically the BTL scores or its transformations–to conduct statistical inference or hypothesis testing. For example, researchers have used these scores to identify 'misfit' judges and stimuli #cite(<Pollitt_2012b>);#cite(<vanDaal_et_al_2016>);#cite(<Goossens_et_al_2018>);, detect biases in judges’ ratings #cite(<Pollitt_et_al_2003>);#cite(<Pollitt_2012b>);, calculate correlations with other assessment methods #cite(<Goossens_et_al_2018>);#cite(<Bouwer_et_al_2023>);, or test hypotheses related to the underlying trait of interest #cite(<Casalicchio_et_al_2015>);#cite(<Bramley_et_al_2019>);#cite(<Boonen_et_al_2020>);#cite(<Bouwer_et_al_2023>);#cite(<vanDaal_et_al_2017>);#cite(<Jones_et_al_2019>);#cite(<Gijsen_et_al_2021>);.

Nevertheless, while separating the trait measurement and hypothesis testing procedures simplifies the analysis of CJ data, the statistical literature cautions against relying solely on the point estimates of BTL scores to conduct statistical inference or hypothesis tests, as this practice can undermine the resulting statistical inferences. A key consideration is that BTL scores are parameter estimates that inherently carry uncertainty \(measurement error). Ignoring this uncertainty can bias the analysis and reduce the precision of hypothesis tests. The direction and magnitude of such biases are often unpredictable. Results may be attenuated, exaggerated, or remain unaffected depending on the degree of uncertainty in the scores and the actual effects being tested #cite(<McElreath_2020>);#cite(<Kline_et_al_2023>);#cite(<Hoyle_et_al_2023>);. Furthermore, the reduced precision in hypothesis tests diminishes their statistical power, increasing the likelihood of committing type-I or type-II errors #cite(<McElreath_2020>);.

In aggregate, the heavy reliance on Thurstone’s Case V assumptions in the statistical analysis of comparative data can compromise the reliability of trait estimates. This overreliance may also undermine their validity #cite(<Perron_et_al_2015>);, particularly when coupled with the disconnect between the trait measurement and hypothesis testing procedures. The structural approach to causal inference can address these issues by offering a systematic and integrated framework to extend Thurstone’s general form. This approach can also strengthen measurement reliability and validity while enhancing the statistical accuracy of hypothesis tests.

= Extending Thurstone’s general form
<sec-theoretical>
The #emph[structural approach] to causal inference #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>) offers a formal framework for identifying causes and estimating their effects using data. The approach relies on structural causal models \(SCMs) and directed acyclic graphs \(DAGs) to formally and graphically represent the assumed causal structure of a system #cite(<Morgan_et_al_2014>);#cite(<Gross_et_al_2018>);#cite(<Neal_2020>);, such as the one found in CJ assessments. In essence, SCMs and DAGs function as #emph[conceptual models] on which identification analysis rests #cite(<Schuessler_et_al_2023>);. #emph[Identification analysis] determines whether an estimator can accurately compute an estimand based solely on its \(causal) assumptions, regardless of random variability. Here, #emph[estimands] represent the specific quantities researchers aim to determine \(i.e., a parameter) #cite(<Everitt_et_al_2010>);. #emph[Estimators] denote the methods or functions that transform data into an estimate \(e.g., a statistical model), while #emph[estimates] are the numerical values approximating the estimand #cite(<Neal_2020>);#cite(<Everitt_et_al_2010>);.

As an illustration, consider a researcher seeking to answer the question: "To what extent do different teaching methods influence students’ ability to produce high-quality written texts?" To investigate this, the researcher designs a CJ assessment by randomly assigning students \(individuals) to two groups, each exposed to a different teaching method. Judges then compare pairs of students’ written texts \(stimuli) to produce a dichotomous outcome reflecting the relative quality of each text \(trait). Based on this setup, researchers can reformulate the research question as the estimand: #emph[On average, is there a difference in the ability to produce high-quality written texts between the two groups of students?] Following standard CJ practices, the researcher would then use point estimates from the BTL model, or its transformations, to approximate this estimand. However, as discussed in @sec-theory-issue1, Thurstone’s Case V and the BTL model exhibit several statistical and measurement limitations. These limitations hinder the model’s ability to identify and accurately estimate a range of estimands relevant to CJ inquiries, including the one described in this illustration.

Fortunately, SCMs and DAGs support identification analysis through two key advantages#footnote[In depth explanation of these topics is beyond the scope of this study, thus, readers seeking a more profound understanding can refer to introductory papers such as #cite(<Pearl_2010>);, #cite(<Rohrer_2018>);, #cite(<Pearl_2019>);, and #cite(<Cinelli_et_al_2020>);, and introductory books like #cite(<Pearl_et_al_2018>);, #cite(<Neal_2020>);, and #cite(<McElreath_2020>) are useful. For more advanced study, seminal papers such as #cite(<Neyman_et_al_1923>);, #cite(<Rubin_1974>);, #cite(<Spirtes_et_al_1991>);, and #cite(<Sekhon_2009>);, along with books such as #cite(<Pearl_2009>);, #cite(<Morgan_et_al_2014>);, and #cite(<Hernan_et_al_2025>);, are recommended.];. First, regardless of complexity, they can represent various causal structures using only five fundamental building blocks #cite(<Neal_2020>);#cite(<McElreath_2024>);. This feature allows the decomposition of complex structures into manageable components, facilitating their analysis. Second, they depict causal relationships in a non-parametric way. This flexibility enables feasible identification strategies without requiring specification of the types of variables, the functional forms relating them, or the parameters of those functional forms #cite(<Pearl_et_al_2016>);.

Thus, using SCMs and DAGs, this study extends Thurstone’s general form to address the issues identified in @sec-theory-issues. This extension combines Thurstone’s core theoretical principles \(see @sec-thurstone_theory) with key CJ assessment design features, such as the selection of judges, stimuli, and comparisons. Specifically, @sec-theory-theoretical_P introduces the #emph[conceptual-population model] \(henceforth CPM), which incorporates these theoretical principles and assumes an idealized setting where researchers observe a #emph[conceptual population] of comparative judgment data–that is, data representing all repeated judgments made by every available judge for each pair of stimuli produced by each pair of individuals in the population. Conversely, @sec-theory-theoretical_SC presents the #emph[sample-comparison model] \(hereafter SCM), which integrates the assessment design features and reflects a more realistic setting where researchers access only a sample of judges, individuals, stimuli, and comparisons from the conceptual population.

== The conceptual-population model \(CPM)
<sec-theory-theoretical_P>
In the CPM, the idealized scenario of a #emph[conceptual population] of comparative data enables the integration of Thurstone’s theoretical principles and provides a foundation for proposing innovations aimed at addressing some of the issues discussed in @sec-theory-issues.

=== Integrating the first theoretical principles
<sec-theory-theoretical_P1>
Before incorporating the first theoretical principles of Thurstone’s theory, it is essential to further define SCMs. SCMs are formal mathematical models characterized by a set of #emph[endogenous] variables $V$, a set of #emph[exogenous] variables $E$, and a set of functions $F$ #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Cinelli_et_al_2020>);. Endogenous variables are those whose causal mechanisms a researcher chooses to model #cite(<Neal_2020>);. In contrast, exogenous variables represent #emph[errors] or #emph[disturbances] arising from omitted factors that the investigator chooses not to model explicitly #cite(<Pearl_2009>);. Lastly, the functions, referred to as #emph[structural equations];, express the endogenous variables as non-parametric functions of other endogenous and exogenous variables. These functions use the symbol '$:=$' to denote the asymmetrical causal dependence between variables and the symbol '$med tack.t med$' to represent #emph[d-separation];, a concept akin to statistical \(conditional) independence.

@fig-cj03_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj03] presents the first theoretical principles embedded in the CPM evaluating the impact of different teaching methods on students’ writing ability. This SCM outlines the relationship between the conceptual-population outcome $(O_(i a h b j k)^(c p))$ and several related variables. The subscripts $i$ and $h$ identify the students who authored the texts \(i.e., the individuals). The indices $a$ and $b$ represent the texts under comparison \(i.e., the stimuli). The index $j$ indicates the judge conducting the comparison, while the index $k$ accounts for assessment conditions where a judge compares the same pair of stimuli multiple times, i.e., a #emph[repeated measures designs] #cite(<Lawson_2015>);. Thus, the indexing system supports comparisons between different texts written by the same student $\( i = h ;$ $a eq.not b \)$ and between texts written by distinct students $\( i eq.not h ;$ where $a = b$ is permitted$\)$, each compared once or repeatedly by all judges $\( j = 1 , dots.h , n_J ;$ $k = 1 , dots.h , n_K ;$ where $n_J > 1$ and $n_K gt.eq 1 \)$. However, it excludes cases where a judge compares a student’s text to itself, whether once or multiple times $\( i = h ;$ $a = b ;$ $j = 1 , dots.h , n_J ;$ $k = 1 , dots.h , n_K ;$ where $n_J > 1$ and $n_K gt.eq 1 \)$, as such comparison lacks practical relevance within the CJ framework. Here, $n_J$ indicates the total number of judges, and $n_K$ denotes the number of repeated judgments each judge performs.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ O_(i a h b j k)^(c p) & := f_O (D_(i a h b j k))\
D_(i a h b j k) & := f_D (T_(i a) , T_(h b) , B_(j k)) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj03_scm>


]
,
  [
#figure([
#box(width: 77%,image("./images/png/CJ_TM_03.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj03_dag>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Conceptual-population model \(CPM), scalar form.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cj03>


In line with Thurstone’s theory, @fig-cj03_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj03] depicts the texts’ discriminal processes $(T_(i a) , T_(h b))$ and their discriminal difference $(D_(i a h b j k))$ \(see @sec-thurstone_theory). Additionally, the SCM incorporates a key CJ design feature: the judges’ biases $(B_(k j))$. This extension builds on the arguments presented in @sec-theory-issue1b, contending that the discriminal difference becomes an observable outcome only through judges’ perceptions. Given that such perceptions may be imperfect–and that each judge may carry some degree of bias #cite(<Pollitt_et_al_2003>);#cite(<vanDaal_et_al_2016>);–it is reasonable that judges’ perceptions \(bias) should be treated as an integral component of the CJ system from the outset, as this leads to a more accurate representation of the data-generating process underlying the pairwise comparisons. This model defines the preliminary set of endogenous variables, $V = { O_(i a h b j k) , D_(i a h b j k) , T_(i a) , T_(h b) , B_(k j) }$, and the preliminary set of structural equations, $F = f_O , f_D$, which capture the non-parametric dependencies among these variables.

Notably, every SCM has an associated DAG #cite(<Pearl_et_al_2016>);#cite(<Cinelli_et_al_2020>);. A DAG is a #emph[graph] consisting of nodes connected by edges, where nodes represent random variables. The term #emph[directed] indicates that edges or arrows extend from one node to another, indicating the direction of causal influence. The absence of an edge implies no direct relationship between the nodes. The term #emph[acyclic] means that the causal influences do not form loops, ensuring the influences do not cycle back on themselves #cite(<McElreath_2020>);. DAGs conventionally depict observed variables as solid black circles and unobserved \(latent) variables as open circles #cite(<Morgan_et_al_2014>);. Although DAGs conventionally omit exogenous variables for simplicity, the DAGs presented in this section includes exogenous variables to improve clarity and reveal potential issues related to conditioning and confounding #cite(<Cinelli_et_al_2020>);.

@fig-cj03_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj03] displays the DAG corresponding to @fig-cj03_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj03], illustrating the expected causal relationships outlined in Thurstone’s theory. The graph shows that the discriminal processes of the texts $(T_(i a) , T_(h b))$ influence their discriminal difference $(D_(i a h b j k))$, which in turn determines the outcome $(O_(i a h b j k)^(c p))$. It also highlights the influence of judges’ biases $(B_(k j))$ on the discriminal difference. Additionally, the DAG differentiates between observed endogenous variables, such as the outcome \(solid black circle), and latent endogenous variables, including the texts’ discriminal processes, their discriminal difference, and the judges’ biases \(open circles).

=== The #emph[conceptual-population] data structure
<sec-theory-theoretical_P2>
Although specifying a data structure is not mandatory when using SCMs and DAGs, defining one improves clarity and facilitates the description of the system. Thus, to re-express the scalar form of the CJ system shown in @fig-cj03 into an equivalent vectorized form, we first define the vectors $I$ and $J$, along with the matrices $I A$ and $J K$, as in Equation \(@eq-mat11). Here, each element of $I$ represents a unique individual $i$ or $h$, where $n_I$ denotes the total number of individuals. Similarly, each element of $J$ corresponds to a unique judge $j$, with $n_J$ indicating the total number of judges. Moreover, each row of $I A$ represents a unique pairing of individuals $i , h$ with stimuli $a , b$. As a result, the matrix $I A$ contains $n_I dot.op n_A$ rows and $2$ columns, where $n_A$ specifies the number of stimuli available per individual. Likewise, each row of $J K$ associates a judge $j$ with a \(repeated) judgment index $k$. Consequently, the matrix $J K$ has $n_J dot.op n_K$ rows and $2$ columns, where $n_K$ indicates the number of repeated judgments each judge makes.

Additionally, we construct the matrix $R$ to map each row of the $I A$ matrix with a corresponding row from the $J K$ matrix. This matrix has $n$ rows and $6$ columns, where $n = binom(n_I dot.op n_A, 2) dot.op n_J dot.op n_K$. Here, the term $binom(n_I dot.op n_A, 2)$ represents the binomial coefficient, which quantifies the total number of unique comparisons possible between every pair of stimuli generated by each pair of individuals in the population. Thus, we define the matrix as in Equation \(@eq-mat11).

#math.equation(block: true, numbering: "(1)", [ $ I = mat(delim: "[", 1; dots.v; i; dots.v; h; dots.v; n_I) ; J = mat(delim: "[", 1; dots.v; j; dots.v; n_J) ; I A = mat(delim: "[", 1, 1; dots.v, dots.v; 1, n_A; dots.v, dots.v; i, a; dots.v, dots.v; h, b; dots.v, dots.v; n_I, 1; dots.v, dots.v; n_I, n_A) ; J K = mat(delim: "[", 1, 1; dots.v, dots.v; 1, n_K; dots.v, dots.v; j, k; dots.v, dots.v; n_J, 1; dots.v, dots.v; n_J, n_K) ; R = mat(delim: "[", 1, 1, 1, 2, 1, 1; dots.v, dots.v, dots.v, dots.v, dots.v, dots.v; 1, 1, 1, 2, 1, n_K; dots.v, dots.v, dots.v, dots.v, dots.v, dots.v; i, a, h, b, j, k; dots.v, dots.v, dots.v, dots.v, dots.v, dots.v; n_I, n_A - 1, n_I, n_A, n_J, 1; dots.v, dots.v, dots.v, dots.v, dots.v, dots.v; n_I, n_A - 1, n_I, n_A, n_J, n_K) $ ])<eq-mat11>

It is easier to visualize the structure of the previously defined vectors and matrices by considering an example. Assuming $n_I = 5$, $n_A = 2$, $n_J = 3$, and $n_K = 3$, the vectors and matrices described in Equation \(@eq-mat11) take the form as in Equation \(@eq-mat13). #math.equation(block: true, numbering: "(1)", [ $ I = mat(delim: "[", 1; 2; 3; 4; 5) ; #h(0em) J = mat(delim: "[", 1; 2; 3) ; #h(0em) I A = mat(delim: "[", 1, 1; 1, 2; 2, 1; 2, 2; 3, 1; 3, 2; 4, 1; 4, 2; 5, 1; 5, 2) ; #h(0em) J K = mat(delim: "[", 1, 1; 1, 2; 1, 3; 2, 1; 2, 2; 2, 3; 3, 1; 3, 2; 3, 3) ; #h(0em) R = mat(delim: "[", 1, 1, 1, 2, 1, 1; 1, 1, 1, 2, 1, 2; 1, 1, 1, 2, 1, 3; dots.v, dots.v, dots.v, dots.v, dots.v, dots.v; 1, 1, 5, 2, 1, 1; 1, 1, 5, 2, 1, 2; 1, 1, 5, 2, 1, 3; dots.v, dots.v, dots.v, dots.v, dots.v, dots.v; 4, 2, 5, 2, 3, 1; 4, 2, 5, 2, 3, 2; 4, 2, 5, 2, 3, 3; 5, 1, 5, 2, 3, 1; 5, 1, 5, 2, 3, 2; 5, 1, 5, 2, 3, 3) $ ])<eq-mat13>

Now, using Equation \(@eq-mat11), we can re-express @fig-cj03_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj03] and @fig-cj03_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj03] in an equivalent vectorized form, as shown in @fig-cj04. In this depiction, the outcome $O_R^(c p)$, the texts’ discriminal difference $D_R$, their discriminal processes $T_(I A)$, and the judges’ biases $B_(J K)$ are represented as vectors rather than scalar values. These vectors capture all the observations from the conceptual population. Specifically, $O_R^(c p)$ and $D_R$ are observed and latent vectors of length $n$, respectively. Moreover, $T_(I A)$ and $B_(J K)$ are latent vectors of lengths $n_I dot.op n_A$ and $n_J dot.op n_K$, respectively.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ O_R^(c p) & := f_O (D_R)\
D_R & := f_D (T_(I A) , B_(J K)) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj04_scm>


]
,
  [
#figure([
#box(width: 87%,image("./images/png/CJ_TM_04.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj04_dag>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Conceptual-population model \(CPM), initial vectorized form.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cj04>


=== Integrating hierarchical structural components
<sec-theory-theoretical_P3>
Building on the principles of Structural Equation Modeling \(SEM) #cite(<Hoyle_et_al_2023>) and Item Response Theory \(IRT) #cite(<Fox_2010>);#cite(<vanderLinden_et_al_2017_I>);, the CPM integrates two #emph[hierarchical structural components] to examine how different #emph[relevant] #footnote[#emph[Relevant variables] are those that satisfy the #emph[backdoor criterion] #cite(<Neal_2020>)[, pp 37];, that is, they belong to a #emph[sufficient adjustment set] #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Morgan_et_al_2014>);. A #emph[sufficient] set \(potentially empty) blocks all non-causal paths between a predictor and an outcome without opening new ones #cite(<Pearl_2009>);. Refer also to footnote 1.] variables–whether observed or latent–affect the primary latent variable of interest #cite(<Everitt_et_al_2010>);. This hierarchical design supports the formulation and testing of hypotheses that account for both the nested structure of stimuli and the uncertainties inherent in trait estimation \(see @sec-theory-issue1b and @sec-theory-issue2 for a discussion of these considerations).

The top branch of @fig-cj09_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj09] illustrates the first component, where #emph[relevant] #footnote[refer to footnote 2.] student-related variables $X_I$, such as teaching method, and students’ idiosyncratic errors $e_I$ causally influence the latent variable representing students’ writing-quality trait $T_I$. The error term $e_I$ captures variations in students’ traits unexplained by $X_I$. Here, $X_I$ is an observed matrix with $n_I$ rows and $q_I$ independent columns \(variables), and both $e_I$ and $T_I$ are latent vectors of length $n_I$. Additionally, this branch shows how $T_I$, along with #emph[relevant] #footnote[refer to footnote 2.] text-related variables $X_(I A)$ \(e.g., text length), and texts’ idiosyncratic errors $e_(I A)$ causally influence the texts’ written-quality trait $T_(I A)$, the first primary latent variable of interest. The error term $e_(I A)$ captures variations in the texts’ traits that remain unexplained by $T_I$ or $X_(I A)$. Here, $X_(I A)$ is an observed matrix with dimensions $n_I dot.op n_A$ rows and $q_(I A)$ independent columns \(variables), while $e_(I A)$ and $T_(I A)$ are latent matrices with $n_I$ rows and $n_A$ columns.

Similarly, the bottom branch of @fig-cj09_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj09] depicts the second component, where #emph[relevant] #footnote[refer to footnote 2.] judge-related variables $Z_J$, such as judgment expertise, and judges’ idiosyncratic errors $e_J$ causally influence the latent variable representing judges’ bias $B_J$. The error $e_J$ captures variations in judges’ bias unexplained by $Z_J$. Here, $Z_J$ is an observed matrix with $n_J$ rows and $q_J$ independent columns \(variables), and both $e_J$ and $B_J$ are latent vectors of length $n_J$. Furthermore, the branch shows how $B_J$, along with #emph[relevant] #footnote[refer to footnote 2.] judgment-related variables $Z_(J K)$ \(e.g., the number of judgments a judge makes), and judgments’ idiosyncratic errors $e_(J K)$ causally influence the judges’ biases associated with each text $B_(J K)$, the second primary latent variable of interest. The error $e_(J K)$ captures variations in judgments unexplained by $B_J$ or $Z_(J K)$. Here, $Z_(J K)$ is an observed matrix with dimension $n_J dot.op n_K$ rows and $q_(J K)$ independent columns \(variables), while $e_(J K)$ and $B_(J K)$ are latent latent matrices with $n_J$ rows and $n_K$ columns

Notably, all variables and functions shown in @fig-cj09_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj09] and @fig-cj09_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj09] are part of the set of endogenous variables $V$, structural equations $F$, and exogenous variables $E$ for the CPM. Additionally, the figures demonstrate that all exogenous variables are independent of one another, as indicated by the relationships $e_(I A) med tack.t med { e_I , e_(J K) , e_J }$, $e_I med tack.t med { e_(J K) , e_J }$ and $e_(J K) med tack.t med e_J$ and the absence of connecting arrows.

Overall, the CPM extends Thurstone’s general form by introducing key innovations to address the limitations discussed in @sec-theory-issue1b and @sec-theory-issue2. These enhancements include accounting for judges’ biases and integrating hierarchical structural components. Nevertheless, despite its promise of enhancing measurement accuracy and precision, the model still depends on the unrealistic assumption that researchers have access to data from the #emph[conceptual population];. Since this assumption is rarely met in practice, the study must consider a more realistic scenario.

#figure([
#block[
#grid(columns: 1, gutter: 2em,
  [
#figure([
$ O_R^(c p) & := f_O (D_R)\
D_R & := f_D (T_(I A) , B_(J K))\
T_(I A) & := f_T (T_I , X_(I A) , e_(I A))\
T_I & := f_T (X_I , e_I)\
B_(J K) & := f_B (B_J , Z_(J K) , e_(J K))\
B_J & := f_B (Z_J , e_J)\
e_I & med tack.t med { e_J , e_(I A) , e_(J K) }\
e_J & med tack.t med { e_(I A) , e_(J K) }\
e_(I A) & med tack.t med e_(J K) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj09_scm>


]
,
  [
#figure([
#box(width: 70%,image("./images/png/CJ_TM_09.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj09_dag>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Conceptual-population model \(CPM), final vectorized form.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cj09>


== The sample-comparison model \(SCM)
<sec-theory-theoretical_SC>
The SCM presents a more realistic scenario than the CPM. First, it explicitly assumes a data sample consisting of a limited number of repeated judgments $(n_K^s)$ from a sample of judges $(n_J^s)$ and a specific number of texts $(n_A^s)$ from a sample of students $(n_I^s)$, all drawn from the conceptual population \(@sec-theory-theoretical_SC1). Second, the model assumes that judges do not perform #emph[all repeated judgments] within the data sample \(@sec-theory-theoretical_SC2). Instead, they conduct a sufficient number of stimuli comparisons, $n_C$, to ensure an accurate estimation of the proportion $P (B > A)$, as proposed by #cite(<Thurstone_1927b>);.

=== The sample mechanism
<sec-theory-theoretical_SC1>
To incorporate the sampling mechanism and facilitate the interpretation of the sample-comparison model, we first define the #emph[data sampling process] using the binary vector variables $S_I$, $S_J$, $S_(I A)$, and $S_(J K)$ as follows: #math.equation(block: true, numbering: "(1)", [ $ S_I = mat(delim: "[", i_((1)); dots.v; i_((i)); dots.v; i_((h)); dots.v; i_((n I))) ; #h(0em) S_J = mat(delim: "[", j_((1)); dots.v; j_((j)); dots.v; j_((n J))) ; #h(0em) S_(I A) = mat(delim: "[", i a_((1 , 1)); dots.v; i a_((1 , n_A)); dots.v; i a_((i , a)); dots.v; i a_((h , b)); dots.v; i a_((n I , 1)); dots.v; i a_((n I , n A))) ; #h(0em) S_(J K) = mat(delim: "[", j k_((1 , 1)); dots.v; j k_((1 , n_K)); dots.v; j k_((j , k)); dots.v; j k_((n J , 1)); dots.v; j k_((n J , n K))) $ ])<eq-mat21>

Where each element of $S_I$ is a binary value indicating the presence or absence of corresponding elements in the vector $I$, as in Equation \(@eq-mat22). We apply the same logic to $S_J$ using vector $J$ \(not shown). Thus, the vectors $S_I$ and $S_J$ contains $n_I$ and $n_J$ elements, respectively. #math.equation(block: true, numbering: "(1)", [ $ i_((i)) = cases(delim: "{", 1 & upright("if data element ") i upright(" from ") I upright(" is sampled"), 0 & upright("if data element ") i upright(" from ") I upright(" is missing")) $ ])<eq-mat22>

Similarly, each element of $S_(I A)$ is a binary value indicating the presence or absence of data rows in the matrices $I A$, as defined in Equation \(@eq-mat23). We apply the same logic to $S_(J K)$ using the matrix $J K$ \(not shown). Thus, the vectors $S_(I A)$ and $S_(J K)$ contains $n_I dot.op n_A$ and $n_J dot.op n_K$ elements, respectively. #math.equation(block: true, numbering: "(1)", [ $ i a_((i , a)) = cases(delim: "{", 1 & upright("if data elements ") i , a upright(" from ") I A upright(" are sampled"), 0 & upright("if data elements ") i , a upright(" from ") I A upright(" are missing")) $ ])<eq-mat23>

We can illustrate the structure of these vectors more clearly with an example. Suppose researchers exclude the second student, the second text from each student, and the third judge from the setup shown in Equation \(@eq-mat13). Given $n_I = 5$, $n_A = 2$, $n_J = 3$, and $n_K = 3$, the resulting vectors would have the following structure: #math.equation(block: true, numbering: "(1)", [ $ S_I = mat(delim: "[", 1; 0; 1; 1; 1) ; #h(0em) S_J = mat(delim: "[", 1; 1; 0) ; #h(0em) S_(I A) = mat(delim: "[", 1; 0; 0; 0; 1; 0; 1; 0; 1; 0) ; #h(0em) S_(J K) = mat(delim: "[", 1; 1; 1; 1; 1; 1; 0; 0; 0) $ ])<eq-mat24>

Notably, Equation \(@eq-mat24) shows that missing observations in the vectors $S_I$ and $S_J$–which represent unsampled students and judges–directly determine which observations are missing in $S_(I A)$ and $S_(J K)$. In other words, researchers can only observe texts and judgments from students and judges initially included in the sample. The equation also shows that the sum of observed elements in $S_I$ equals the number of sampled students $(n_I^s)$ and that a similar sum in vector $S_J$ equals the sampled judges $(n_J^s)$. Conversely, the sum of observed elements in $S_(I A)$ represents the total sampled texts across all sampled students $(n_I^s dot.op n_A^s)$, while a similar sum in vector $S_(J K)$ represents the total sampled repeated judgments across all sampled judges $(n_J^s dot.op n_K^s)$. Notice that in this example, because the design systematically excludes every third repeated judgment, $S_(J K)$ can also be expressed using $n_K = n_K^s = 2$.

Finally, we define the #emph[sample mechanism] $S$ in Equation \(@eq-mat25), which maps each element of $S_(I A)$ to every element of $S_(J K)$. Each element $s_((i , a , h , b , j , k))$ is a binary value indicating the presence or absence of data rows in the matrix $R$ resulting from the sample mechanism, as in Equation \(@eq-mat26). Thus, the vector contains $n$ elements, matching the number of rows in $R$, and the sum of its elements represents the total data sample: $n^s = binom(n_I^s dot.op n_A^s, 2) dot.op n_J^s dot.op n_K^s$. Here, the term $binom(n_I^s dot.op n_A^s, 2)$ represents the binomial coefficient, which quantifies the total number of unique comparisons possible between every pair of sampled stimuli generated by each pair of sampled individuals.

#math.equation(block: true, numbering: "(1)", [ $ s_((i , a , h , b , j , k)) = cases(delim: "{", 1 & upright("if data elements ") i , a , h , b , j , k upright(" from ") R upright(" are sampled"), 0 & upright("if data elements ") h , i , a , b , j , k upright(" from ") R upright(" are missing")) $ ])<eq-mat26>

#math.equation(block: true, numbering: "(1)", [ $ S = mat(delim: "[", s_((1 , 1 , 1 , 2 , 1 , 1)); dots.v; s_((1 , 1 , 1 , 2 , 1 , n_K)); dots.v; s_((i , a , h , b , j , k)); dots.v; s_((n_I , n_A - 1 , n_I , n_A , n_J , 1)); dots.v; s_((n_I , n_A - 1 , n_I , n_A , n_J , 1))) $ ])<eq-mat25>

With the definition of $S$, we incorporate the sample mechanism into the CPM. Following the convention of #cite(<McElreath_2020>) and #cite(<Deffner_et_al_2022>);, @fig-cj14_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj14] represents the conceptual-population outcome $O_R^(c p)$ as unobserved, emphasizing that this outcome cannot be directly accessed due to the sampling mechanism. The DAG also depicts the #emph[sample design] vector $S$ as a causal factor influencing the sample-comparison outcome $O_R^(s c)$. A square encloses $S$, indicating that it is a conditioned variable. In this context, #emph[conditioning] means that the analysis is restricted to the elements of $O_R^(c p)$ that satisfy $s_((i , a , h , b , j , k)) = 1$ #cite(<Neal_2020>);#cite(<McElreath_2020>);. In essence, $S$ is a vector that selects #emph[all repeated judgments made by a subset of judges for a subset of stimuli produced by the sampled individuals];.

Notably, the DAG shows that $S$ is independent of all other variables in the model. This implies that @fig-cj14_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj14] applies exclusively to Simple Random Sampling \(SRSg) designs. In these designs, each repeated judgment, judge, stimulus, and individual has the same probability of being included in the sample as any other observation within their respective groups #cite(<Lawson_2015>);.

However, due to concerns about the practical feasibility of the comparison task #cite(<Boonen_et_al_2020>);, CJ assessments rarely implement an exhaustive pairings of sampled judges, stimuli, and individuals. Thus, a realistic scenario must account for the fact that judges typically compare only a subset of stimuli authored by a sample of individuals.

#figure([
#block[
#grid(columns: 1, gutter: 2em,
  [
#figure([
$ O_R & := f_C (O_R^(s c) , C)\
O_R^(s c) & := f_S (O_R^(c p) , S)\
O_R^(c p) & := f_O (D_R)\
D_R & := f_D (T_(I A) , B_(J K))\
T_(I A) & := f_T (T_I , X_(I A) , e_(I A))\
T_I & := f_T (X_I , e_I)\
B_(J K) & := f_B (B_J , Z_(J K) , e_(J K))\
B_J & := f_B (Z_J , e_J)\
e_I & med tack.t med { e_J , e_(I A) , e_(J K) }\
e_J & med tack.t med { e_(I A) , e_(J K) }\
e_(I A) & med tack.t med e_(J K) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj14_scm>


]
,
  [
#figure([
#box(width: 100%,image("./images/png/CJ_TM_14.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj14_dag>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Sample-comparison model, final vectorized form
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cj14>


=== The comparison mechanism
<sec-theory-theoretical_SC2>
As in the previous section, we begin defining the #emph[comparison mechanism] using the binary vector variable $C$ to facilitate the interpretation of the sample-comparison model. Equation \(@eq-mat27) shows that $C$ contains $n$ elements corresponding to the number of rows in the $R$ matrix, with each element $c_((i , a , h , b , j , k))$ being a binary value indicating the presence or absence of data rows in $R$, a definition similar to that of $s_((i , a , h , b , j , k))$ in Equation \(@eq-mat26). #math.equation(block: true, numbering: "(1)", [ $ C = mat(delim: "[", c_((1 , 1 , 1 , 2 , 1 , 1)); dots.v; c_((1 , 1 , 1 , 2 , 1 , n_K)); dots.v; c_((i , a , h , b , j , k)); dots.v; c_((n_I , n_A - 1 , n_I , n_A , n_J , 1)); dots.v; c_((n_I , n_A - 1 , n_I , n_A , n_J , 1))) $ ])<eq-mat27>

The @fig-cj14_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj14] also incorporates the #emph[comparison mechanism] $C$ into the CPM. It shows the sample-comparison outcome $O_R^(s c)$ as unobserved, emphasizing that this variable cannot be directly accessed because of the comparison mechanism. The DAG further shows $C$ as a conditioned variable \(enclosed in a square) that causally influences the observed outcome $O_R$. This structure implies that $C$ determines #emph[which repeated judgments judges make for the stimuli produced by the individuals];. In essence, $C$ reflects the assumption that judges #emph[do not] perform all possible repeated judgments but instead complete a sufficient number, $n_C$, to enable the accurate estimation of the proportion $P (B > A)$ for each stimulus pair #cite(<Thurstone_1927b>)[, p.~267];.

Notably, @fig-cj14_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj14] also shows that $C$ is independent of all other variables in the model. This independence implies that the conceptual model represented by the DAG applies exclusively to Random Allocation Comparative Designs #cite(<Bramley_2015>);, or Incomplete Block Designs #cite(<Lawson_2015>);, where every repeated judgment has an equal probability of being included in the sample.

Finally, since it is standard to assume that the distribution of the conceptual-population outcome $O_R^(c p)$ also holds for $O_R^(s c)$ and $O_R$, we can reformulate the sample-comparison model in @fig-cj14 into the equivalent form shown in @fig-cj15. This reformulation produces a model that applies directly to a sample of comparative data. In this version, the unobserved outcomes $O_R^(c p)$ and $O_R^(s c)$ are omitted, and $O_R$ inherits the structural equation $f_O$ that originally defined $O_R^(c p)$. Moreover, the definition of $O_R$ now reflects its direct dependence on the discriminal difference $D_R$ and the sample and comparison mechanisms, $S$ and $C$.

#figure([
#block[
#grid(columns: 1, gutter: 2em,
  [
#figure([
$ O_R & := f_O (D_R , S , C)\
D_R & := f_D (T_(I A) , B_(J K))\
T_(I A) & := f_T (T_I , X_(I A) , e_(I A))\
T_I & := f_T (X_I , e_I)\
B_(J K) & := f_B (B_J , Z_(J K) , e_(J K))\
B_J & := f_B (Z_J , e_J)\
e_I & med tack.t med { e_J , e_(I A) , e_(J K) }\
e_J & med tack.t med { e_(I A) , e_(J K) }\
e_(I A) & med tack.t med e_(J K) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj15_scm>


]
,
  [
#figure([
#box(width: 72%,image("./images/png/CJ_TM_15.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj15_dag>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Comparative judgment model
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cj15>


In summary, the @fig-cj15_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj15] and @fig-cj15_dag[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj15] extend Thurstone’s general form to address several limitations of the BTL model. These extensions account for judges’ biases \(see @sec-theory-theoretical_P1), reflect the hierarchical structure of stimuli and incorporate measurement error in trait estimation and hypothesis testing \(see @sec-theory-theoretical_P3), and even clarify the role of the sample and comparison mechanisms in CJ assessments \(see @sec-theory-theoretical_SC). However, they do not resolve concerns about the assumption of equal dispersions among stimuli discussed in @sec-theory-issue1a. Since this concern relates to the statistical assumption underlying the distribution of the discriminal process, we develop a formal statistical model to address it in the next section.

= From SCM to statistical model
<sec-statistical>
Using the @fig-cj15_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj15], we can derive a statistical model that addresses violations of the equal dispersion assumption \(see @sec-theory-issue1a). This derivation is possible because a fully specified SCM encodes functional and probabilistic information, which we can replace with suitable functions and probabilistic assumptions #cite(<Pearl_et_al_2016>);. Specifically, @fig-cj15_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj15] allows us to express the joint distribution of our complex CJ system as a product of simpler conditional probability distributions \(CPDs)#footnote[This re-expression is possible because the #emph[chain rule] of probability and the #emph[Bayesian Network Factorization \(BNF)] property. For further details, see #cite(<Pearl_et_al_2016>) and #cite(<Neal_2020>);.];, as shown in Equation \(@eq-mat51). For clarity, we treat expressions such as $Y := f_Y (X)$, $P (Y divides X)$, and $Y tilde.op f (Y divides X)$ as equivalent, where $P (Y divides X)$ and $f (Y divides X)$ represent the CPD of $Y$ given $X$. #math.equation(block: true, numbering: "(1)", [ $ P \( O_R , & S , C , D_R , T_(I A) , X_(I A) , e_(I A) , T_I , X_I , e_I , B_(J K) , Z_(J K) , e_(J K) , B_J , Z_J , e_J \) & \
 & = P (O_R divides D_R , S , C) dot.op P (S) dot.op P (C) dot.op P (D_R divides T_(I A) , B_(J K))\
 & quad dot.op P (T_(I A) divides T_I , X_(I A) , e_(I A)) dot.op P (T_I divides X_I , e_I)\
 & quad dot.op P (B_(J K) divides B_J , Z_(J K) , e_(J K)) dot.op P (B_J divides Z_J , e_J)\
 & quad dot.op P (X_(I A)) dot.op P (X_I) dot.op P (Z_(J K)) dot.op P (Z_J)\
 & quad dot.op P (e_(I A)) dot.op P (e_I) dot.op P (e_(J K)) dot.op P (e_J) $ ])<eq-mat51>

Each CPD in Equation \(@eq-mat51) rests on specific assumptions, which we outline in the statistical model presented in @fig-cj16_stat[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj16]. The model starts by assuming that $O_R$ follows a Bernoulli distribution#footnote[The binomial distribution–including its special case, the Bernoulli distribution–represent a maximum entropy distribution for binary events #cite(<McElreath_2020>)[, p.~34];. This means that the Bernoulli distribution is the most consistent alternative when only two un-ordered outcomes are possible and their expected frequencies are assumed to be constant #cite(<McElreath_2020>)[, p.~310];. For a detailed discussion of the binomial as a maximum entropy distribution, see #cite(<McElreath_2020>)[sec.~10.1.2];.];, reflecting the binary nature of CJ outcomes. Furthermore, following the conventions of Generalized Linear Models \(GLMs) #cite(<Nelder_et_al_1983>);#cite(<Nelder_et_al_1996>);#cite(<Agresti_2015>);, the distribution links $O_R$ to the latent discriminal difference vector $D_R$ using an inverse-logit function: $upright("inv_logit") (x) = 1 \/ (1 + exp (- x))$.

#figure([
#block[
#grid(columns: 3, gutter: 2em,
  [
#figure([
$ O_R & := f_O (D_R , S , C)\
D_R & := f_D (T_(I A) , B_(J K))\
T_(I A) & := f_T (T_I , X_(I A) , e_(I A))\
T_I & := f_T (X_I , e_I)\
B_(J K) & := f_B (B_J , Z_(J K) , e_(J K))\
B_J & := f_B (Z_J , e_J)\
\
e_I & med tack.t med { e_J , e_(I A) , e_(J K) }\
e_J & med tack.t med { e_(I A) , e_(J K) }\
e_(I A) & med tack.t med e_(J K) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj16_scm>


]
,
  [
#figure([
$  & P (O_R divides D_R , S , C)\
 & P (D_R divides T_(I A) , B_(J K))\
 & P (T_(I A) divides T_I , X_(I A) , e_(I A))\
 & P (T_I divides X_I , e_I)\
 & P (B_(J K) divides B_J , Z_(J K) , e_(J K))\
 & P (B_J divides Z_J , e_J)\
\
 & P (e_I) P (e_(I A)) P (e_J) P (e_(J K))\
\
\
 $

], caption: figure.caption(
position: bottom, 
[
Probabilistic model
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj16_prob>


]
,
  [
#figure([
$ O_R & tilde.op^(i i d) upright("Bernoulli") [upright("inv_logit") (D_R)]\
D_R & = (T_(I A) [i , a] - T_(I A) [h , b]) + B_(J K) [j , k]\
T_(I A) & = T_I + beta_(X A) X_(I A) + e_(I A)\
T_I & = beta_(X I) X_I + e_I\
B_(J K) & = B_J + beta_(Z K) Z_(J K) + e_(J K)\
B_J & = beta_(Z J) Z_J + e_J\
\
bold(e) & tilde.op upright("Multi-Normal") (bold(mu) , bold(Sigma))\
bold(Sigma) & = bold(V) bold(Q) bold(V)\
\
 $

], caption: figure.caption(
position: bottom, 
[
Statistical model
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-cj16_stat>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Comparative judgment model, SCM, probabilistic and statistical model assuming different discriminal dispersions for the student’s traits
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cj16>


While the joint distribution in Equation \(@eq-mat51) includes the probability distributions of the sampling and comparison mechanisms, $P (S)$ and $P (C)$, as well as those of the predictor variables–$P (X_(I A))$, $P (X_I)$, $P (Z_(J K))$, and $P (Z_J)$–all of these probabilities are omitted from the statistical model @fig-cj16_stat[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj16]. This omission is justified because, while these distributions contribute to the overall joint distribution of the data, the variables $S$, $C$, $X_(I A)$, $X_I$, $Z_(J K)$, and $Z_J$ are observed and independent of any other variable in the model. As observed variables, they do not require distributional assumptions in the same way the idiosyncratic errors do. Their independence follows from the underlying random selection procedures that govern the variables#footnote[Randomization ensures that data–and, by extension, an estimator–satisfies several key identification properties, such as common support, no interference, and consistency. The most critical property, however, is the elimination of confounding. #emph[Confounding] occurs when an external variable, such as $X_I$, simultaneously influences both the outcome \(e.g., $O_R$) and a variable of interest \(e.g., $S$), resulting in spurious associations between the latter two #cite(<Everitt_et_al_2010>);. Randomization ensure the absence of confounding by effectively decoupling the association between the variable of interest and any other variable, except for the outcome itself. For a more detailed discussion on the benefits of randomization, see #cite(<Pearl_2009>);, #cite(<Morgan_et_al_2014>);, #cite(<Neal_2020>);, and #cite(<Hernan_et_al_2025>);.];.

Next $D_R$ is defined as the difference between the discriminal processes $T_(I A) [i , a]$ and $T_(I A) [h , b]$, representing the underlying written-quality trait of the compared texts, plus the corresponding repeated judge bias $B_(J K) [j , k]$. Note that if it is assumed that $B_(J K) [j , k]$ reflects the difference in stimulus-specific biases, i.e., $B_(J K) [j , k] = B_(J K) [i , a , j , k] - B_(J K) [h , b , j , k]$, the discriminal difference can be re-written as: #math.equation(block: true, numbering: "(1)", [ $ D_R & = (T_(I A) [i , a] - T_(I A) [h , b]) + B_(J K) [j , k]\
 & = (T_(I A) [i , a] + B_(J K) [i , a , j , k]) - (T_(I A) [h , b] + B_(J K) [h , b , j , k])\
 & = T_(I A)^(\*) [i , a] - T_(I A)^(\*) [h , b] $ ])<eq-mat52>

This formulation reveals that the discriminal difference captures a #emph[pure interaction effect];, in which neither the texts’ discriminal processes nor the judges’ biases alone determine the outcome, but their interaction does #cite(<Attia_et_al_2022>);. Put simply, this mathematical description captures the idea that the stimuli’ discriminal processes become an observable outcome only through the lens of judges’ perceptions \(i.e., their biases). For clarity, the square brackets in $D_R$ indicate the relevant indices for each trait vector; they do not imply any subsetting of the data.

Now the functional forms for $T_(I A)$, $T_I$, $B_(J K)$, and $B_J$ are specified. $T_(I A)$ is modeled as a linear combination of the students’ underlying writing-quality traits $T_I$, the effects of relevant text-related variables on quality assessment $beta_(X A) X_(I A)$ \(such as the influence of text length), and the text-specific idiosyncratic errors $e_(I A)$. Similarly, $T_I$ is expressed as a linear combination of relevant student-related variables affecting the quality assessment $beta_(X I) X_I$, and student-specific idiosyncratic errors $e_I$. For the judge-specific terms, $B_(J K)$ is modeled as a linear combination of the judge’s individual bias $B_J$, the influence of relevant judgment-related variables on quality assessment $beta_(Z K) Z_(J K)$ \(e.g., how the number of judgments affect the evaluation), and judgment-specific idiosyncratic errors $e_(J K)$. Finally, $B_J$ is defined as a linear combination of relevant judge-level variables influencing the quality assessment $beta_(Z J) Z_J$ \(such as judgment expertise) and judge-specific idiosyncratic errors $e_J$.

Next, the probabilistic assumptions for the idiosyncratic errors $e_I$, $e_(I A)$, $e_J$, and $e_(J K)$ are specified. Unlike other variables in the model, these error terms exhibit indeterminacies in their #emph[location];, #emph[orientation];, and #emph[scale] due to the lack of an inherent scale in the associated latent variables $T_I$, $T_(I A)$, $B_J$, and $B_(J K)$. Thus, to identify the latent variable model these indeterminacies must be resolved #cite(<Depaoli_2021>);#cite(<deAyala_2009>);. Drawing on principles from SEM #cite(<Hoyle_et_al_2023>);, the vector of idiosyncratic errors $bold(e) = [e_I , e_(I A) , e_J , e_(J K)]^T$ are assumed to follow a Multivariate Normal distribution with mean vector $bold(mu)$ and a covariance matrix $bold(Sigma) = bold(V) bold(Q) bold(V)$, with $bold(V)$ denoting a diagonal matrix of standard deviations and $bold(Q)$ a correlation matrix. To address the #emph[location] indeterminacy, the errors’ mean vector is set to zero: #math.equation(block: true, numbering: "(1)", [ $ bold(mu) = [0 , 0 , 0 , 0]^T $ ])<eq-mat53>

Following @fig-cj16_scm[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj16], the #emph[orientation] indeterminacy is solved by assuming that the errors are uncorrelated. This assumption leads to the definition of the error’s correlation matrix, $bold(Q)$, as the identity matrix: #math.equation(block: true, numbering: "(1)", [ $ bold(Q) = mat(delim: "[", 1, 0, 0, 0; 0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1) $ ])<eq-mat54>

To resolve the #emph[scale] indeterminacy, the diagonal matrix $bold(V)$ is defined as follows: #math.equation(block: true, numbering: "(1)", [ $ bold(V) = mat(delim: "[", s_(X I), 0, 0, 0; 0, p_(I A), 0, 0; 0, 0, s_(Z J), 0; 0, 0, 0, p_(J K)) $ ])<eq-mat55>

Here, $s_(X I)$ represents the standard deviation for the individuals, $p_(I A)$ for the stimuli, $s_(Z J)$ for the judges, and $p_(J K)$ for the judgments. It is assumed $s_(X I)$ varies depending on the teaching method group to which each student belongs. Using the example from @sec-theoretical, where the teaching method $X_I = { 1 , 2 }$, the model sets the constraint according to Equation \(@eq-mat56). This constraint anchors the scale of the individuals’ latent trait while relaxing the assumption of equal dispersion for the stimuli, thereby addressing the concerns raised in @sec-theory-issue1a. #math.equation(block: true, numbering: "(1)", [ $ sum_(g = 1)^2 s_(X I) [g] \/ 2 = 1 $ ])<eq-mat56>

Because the error vector $bold(e)$ follows an uncorrelated Multivariate Normal distribution, the marginal distribution of $e_(I A)$ is a univariate Normal distribution with mean zero and standard deviation $p_(I A)$. Thus, $p_(I A)$ is set as a proportion of $1$ to establish the scale of the stimuli’ latent trait relative to the scale of the individuals’ trait. Note that as a result, $T_(I A)$ is also normally distributed. This configuration effectively reinstates Thurstone’s original assumption of Normal discriminal processes for the stimuli \(see @tbl-thurstone_cases).

Similarly, it is assumed that $s_(Z J)$ varies depending on the groups to which each judge belongs. For instance, if $Z_J = { 1 , 2 , 3 }$ represents three groups of judges with varying expertise, the model sets the constraint according to Equation \(@eq-mat57). This constraint anchors the scale of the judges’ latent trait and relaxes the assumption of equal dispersion for the judgments. #math.equation(block: true, numbering: "(1)", [ $ sum_(g = 1)^2 s_(Z J) [g] \/ 3 = 1 $ ])<eq-mat57>

Conversely, $p_(J K)$ is defined as a proportion of $1$ to establish the scale of the judgments’ latent trait relative to the scale of the judges’ trait.

Finally, we use #emph[Bayesian inference methods] to convert the statistical model @fig-cj16_stat[45127368-afa1-446a-820f-fc64c546b2c5%fig-cj16] into a practical statistical tool for analyzing paired comparison data. Bayesian inference offers three main advantages in this context. First, it handles complex and overparameterized models, where the number of parameters exceeds the number of observations #cite(<Baker_1998>);#cite(<Kim_et_al_1999>);. This feature is essential for our implementation, as the proposed model is indeed overparameterized. Second, it incorporates prior information to constrain parameter estimates within plausible bounds, thereby mitigating estimation issues like non-convergence or improper solutions that often affect frequentist methods #cite(<Martin_et_al_1975>);#cite(<Seaman_et_al_2011>);. Prior distributions are used to define the error distribution and set the scale of latent variables #cite(<Depaoli_2014>);. Third, Bayesian inference supports robust inferences from small samples, where the asymptotic properties underlying frequentist methods are less reliable #cite(<Baldwin_et_al_2013>);#cite(<Lambert_et_al_2006>);#cite(<Depaoli_2014>);. This feature is particularly relevant in CJ assessments, as researchers often collect large volumes of paired comparisons but work with relatively small samples of judges, stimuli, and individuals to test hypotheses.

The #strong[Declarations] section of this document provides a link to the model code, along with an alternative specification that assumes equal discriminal dispersions. We tested both versions of the model with success using `Stan` #cite(<Stan_2020>)[, version 2.26.1];.

= Discussion
<sec-discussion>
Thurstone introduced the Law of Comparative Judgment to measure psychological traits of stimuli through pairwise comparisons #cite(<Thurstone_1927a>);#cite(<Thurstone_1927b>);. In its general form, the theory models single-judge comparisons across multiple, potentially correlated stimuli. Each comparison produces a dichotomous outcome indicating which stimulus the judge perceives as having a higher trait level. However, Thurstone identified one key challenge in this general formulation: the measurement model required estimating more "unknown" parameters than the number of available pairwise comparisons #cite(<Thurstone_1927b>);. To address this issue and to facilitate the theory’s practical applicability, he formulated five cases, each progressively incorporating several simplifying assumptions.

Among these, Case V remains the most widely used model in empirical CJ research, mainly due to the widespread adoption of the BTL model. The BTL model incorporates the core assumptions of Case V–namely, equal discriminal dispersions and zero correlation among stimuli’ discriminal processes–but replaces the processes’ normal distribution with the more mathematically tractable logistic distribution #cite(<Andrich_1978>);#cite(<Bramley_2008>);. Although this substitution has minimal impact on trait estimation or model interpretation #cite(<vanderLinden_et_al_2017_I>);#cite(<McElreath_2021>);, the simplifying assumptions of the BTL model–and by extension, of Case V–may fail to capture the complexity of some traits or account for heterogeneous stimuli #cite(<Thurstone_1927a>);#cite(<Andrich_1978>);#cite(<Bramley_2008>);#cite(<Kelly_et_al_2022>);, potentially leading to unreliable and inaccurate trait estimates #cite(<Ackerman_1989>);#cite(<Zimmerman_1994>);#cite(<McElreath_2020>);#cite(<Hoyle_et_al_2023>);.

Moreover, because Thurstone’s original goal was to produce a "coarse scaling" of traits and allocate stimuli along this continuum #cite(<Thurstone_1927a>)[, p.~269];, his theory offered no guidance on how to use trait estimates for statistical inference. The CJ tradition has attempted to address this gap by separating trait estimation from hypothesis testing, relying on point estimates, such as BTL scores or their transformations, for inference. While this approach simplifies analysis, it can also introduce bias and compromise the reliability of the resulting inferences #cite(<McElreath_2020>);#cite(<Kline_et_al_2023>);#cite(<Hoyle_et_al_2023>);.

To address the limitations of Thurstone’s Case V and the BTL model, this study extended Thurstone’s general form using a systematic, integrated approach that combined causal and Bayesian inference methods. The approach began with the development of a conceptual model, formalized as a Structural Causal Model \(SCM) and represented graphically by a Directed Acyclic Graph \(DAG) #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Gross_et_al_2018>);#cite(<Neal_2020>);. This model integrated Thurstone’s core theoretical principles, such as the discriminal processes of stimuli, alongside key CJ assessment design features, including judges’ bias, sampling procedures, and comparison mechanisms. Together, these components allowed the causal processes underlying the CJ system to be disentangled.

The approach then translated the SCM into a bespoke statistical model that enabled the analysis of CJ data in cases where the assumptions of equal dispersion and zero correlation were violated, and where statistical inference was required. In particular, the model accounted for judge biases, captured the hierarchical structure of stimuli, incorporated measurement error into the hypothesis testing process, and accommodated heterogeneity in discriminal dispersions. By addressing these issues, the methodological innovations aimed to enhance the reliability and validity of trait measurement in CJ, while also improving the accuracy of statistical inferences.

Beyond these potential benefits, the approach offered two additional advantages. First, it clarified the roles and interactions of all actors and processes involved in CJ assessments. Second, it shifted the analytic paradigm from passively accepting the assumptions of Case V and the BTL model to actively testing their fit with observed data. Together, these advantages established a principled framework for evaluating best practices in designing CJ assessments–one that better aligns with the demands of contemporary CJ contexts #cite(<Kelly_et_al_2022>);–providing new insights into existing research and opening promising avenues for future inquiry.

== Future research directions using our approach
<sec-discussion_RA>
Among the many potential directions for future research, three avenues deserve particular attention due to their direct impact on the reliability and validity of CJ trait estimates, as well as on the accuracy of statistical inferences. The following sections outline these avenues and explain how our approach facilitates their investigation.

=== The impact of sampling and comparison mechanisms
<sec-discussion_RA1>
Although sampling and comparison mechanisms are central to modern CJ assessments, it is striking that most CJ literature has examined them within a limited scope. Researchers have primarily investigated the effects of adaptive comparative judgment \(ACJ) designs on trait reliability #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);#cite(<Bramley_2015>);#cite(<Verhavert_et_al_2022>);#cite(<Mikhailiuk_et_al_2021>);#cite(<Gray_et_al_2024>) or proposed practical guidelines for the number of comparisons judges should make #cite(<Verhavert_et_al_2019>);#cite(<Crompvoets_et_al_2022>);. While these studies offer valuable insights, they also overlook the broader role that these mechanisms play within the CJ system. As this oversight likely stems from a more fundamental lack of conceptual clarity about how these mechanisms function within the system, the present study integrated these mechanisms into the conceptual model of CJ.

The explicit integration of the sampling and comparison mechanisms offers a new perspective on how these mechanisms shape the CJ process. Specifically, it clarified their role as sources of missing data in CJ’s data-generating process–that is, as mechanisms that determine which observations are missing from the final data sample. This new perspective invites the application of Little and Rubin’s principled missing data framework #cite(<Little_et_al_2020>);, allowing a more rigorous evaluation of existing claims about these missing data mechanisms, their influence on CJ outcomes, and their implications for designing and evaluating more complex assessments setups.

This study circumvented the need to apply the missing data framework by deliberately structuring the sampling and comparison mechanisms to be independent of any observed or unobserved variables, including the outcome. In other words, these mechanisms were intentionally designed to yield data that are #emph[missing completely at random] \(MCAR) #cite(<Little_et_al_2020>);. This design choice offered one key advantage: it generated simple random samples that satisfied the condition of #emph[ignorability];, thereby allowing researchers to legitimately #emph[ignore] missing data during analysis without introducing bias #cite(<Everitt_et_al_2010>);#cite(<Kohler_et_al_2019>);#cite(<Neal_2020>);.

However, many modern CJ applications rely on more complex assessment designs, in which the sampling and comparison mechanisms introduce more intricate forms of missingness such as #emph[missing at random] \(MAR) or #emph[missing not at random] \(MNAR) #cite(<Little_et_al_2020>);. One prominent example is ACJ designs, where prior judgment outcomes inform the selection of stimulus pairs for subsequent comparisons #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);#cite(<Bramley_2015>);. This pair selection procedure suggests that ACJ’s comparison mechanism is outcome-dependent, potentially classifying the method as a generator of MNAR data. If this classification holds, ACJ might violate the condition of ignorability, making it an unsuitable pair selection procedure for reliable and valid trait estimation and inference. Moreover, under this interpretation, the mixed findings on ACJ’s effectiveness become more comprehensible: while some studies find that the method improves trait reliability #cite(<Pollitt_et_al_2003>);#cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);, others argue that that these gains may be artificially inflated #cite(<Bramley_2015>);#cite(<Bramley_et_al_2019>);#cite(<Crompvoets_et_al_2020>);#cite(<Crompvoets_et_al_2022>);.

Regardless of the underlying missingness mechanisms, any CJ assessment design would benefit from explicitly defining its assumptions–a practice supported by this study’s approach. This clarity enables a thorough evaluation of how the sampling and comparison mechanisms affect trait estimation and statistical inference in each design. Such assessments are particularly relevant given the common misconception in the CJ literature that Thurstone’s model can naturally handle even non-random missing data without compromising the reliability or validity of trait estimates #cite(<Bramley_2008>);.

=== The effects of judges’ bias on the reliability of trait estimates
<sec-discussion_RA2>
Despite the growing notion that various stimulus-related factors influence judges’ perceptions #cite(<vanDaal_et_al_2016>);#cite(<Lesterhuis_et_al_2018>);#cite(<Chambers_et_al_2022>) and that these influences may not always cancel each other out, empirical evidence of judges’ biases remains scarce in the CJ literature #cite(<Pollitt_et_al_2003>);#cite(<vanDaal_et_al_2016>);#cite(<Bartholomew_et_al_2020a>);. This gap likely persists not from a lack of interest or research but because the identification of such biases often depends on ad-hoc detection methods, like 'misfit' statistics, that may not be well-suited for the task #cite(<Kelly_et_al_2022>);. To overcome this limitation, the present study treated judges’ biases as an integral component of the CJ system from the outset. This approach offers one key advantage: it provides a more accurate representation of the data-generating process behind pairwise comparisons, one that acknowledges that the discriminal processes of stimuli becomes an observable outcome only through judges’ perceptions, which may exhibit bias.

The explicit integration of judges’ bias into CJ’s conceptual model then paves the way for investigating several relevant research questions. One key question is whether CJ data can be validly analyzed under the assumption of "sample-free" trait calibration, specifically under the hypothesis that judges exhibit no systematic bias. This question is particularly relevant because "sample-freeness" is still regarded as an inherent property of the BTL model #cite(<Bramley_2008>);#cite(<Andrich_1978>) despite growing evidence of persistent biases. Another critical question is whether training or expertise can help judges avoid focusing on irrelevant stimulus features, such as handwriting, over more central criteria like argumentative quality in writing assessments #cite(<Kelly_et_al_2022>);. Exploring these questions may also provide insights into what it truly means to be an "expert" within the CJ context #cite(<Kelly_et_al_2022>);.

Moreover, since judges rely on these stimulus-related factors when evaluating complex, multidimensional traits \(e.g., structure, style) #cite(<vanDaal_et_al_2016>);#cite(<Lesterhuis_et_al_2018>);#cite(<Chambers_et_al_2022>);, and these factors account for variation in judgment accuracy #cite(<Gill_et_al_2013>);#cite(<vanDaal_et_al_2017>);#cite(<vanDaal_2020>);#cite(<Gijsen_et_al_2021>);, it is reasonable to expect that assessments also vary according to judge-specific attributes such as gender, age, culture, income, education, training, or expertise #cite(<Kelly_et_al_2022>);. Prior studies support this view #cite(<Bartholomew_et_al_2020a>);#cite(<McMahon_et_al_2015>);. Thus, building on the discussion in @sec-discussion_RA1, further exploration is warranted into how judge selection influences the formation of a "shared consensus" and whether certain judge attributes introduce systematic biases or distortions in the stimuli trait distribution #cite(<Deffner_et_al_2022>);. Furthermore, if such attributes do in fact compromise the assumption of "sample-freeness," it becomes essential to consider strategies for mitigating their effects and to determine how many judges \(and how many judgments per judge) are required to produce reliable trait estimates under these conditions. Additionally, it is worth examining whether #emph[repeated measures designs];, in which judges evaluate the same stimulus pairs multiple times #cite(<Lawson_2015>);, can improve judgment consistency and accuracy. As anticipated, the approach presented in this study provides a structured foundation to rigorously investigate these questions.

=== The identification of 'misfitting' judges and stimuli
<sec-discussion_RA3>
Although the CJ literature clearly defines #emph[misfit] judges and stimuli, CJ researchers have rarely examined how these observations relate to Thurstonian theory. In particular, they have not identified which elements of Thurstone’s theory account for the occurrence of misfits. This disconnect likely stems from the fact that misfit statistics are derived from residual analysis and outlier detection methods rather than from Thurstonian principles. Specifically, #emph[misfit judges] are typically defined as those whose assessments diverge significantly from the "shared consensus" #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);#cite(<vanDaal_et_al_2016>);#cite(<Goossens_et_al_2018>);#cite(<Wu_et_al_2022>);, while #emph[misfit stimuli] are those that elicit more judgment discrepancies than others #cite(<Pollitt_2004>);#cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);#cite(<Goossens_et_al_2018>);. Both definitions closely mirror the statistical concept of outliers, that is, observations that deviate markedly from the rest of the sample in which they occur #cite(<Grubbs_1969>);. But this resemblance extends beyond the definitions themselves, as misfits are often identified using conventional outlier detection procedures, such as transforming BTL model residuals into diagnostic statistics and comparing them against predefined thresholds #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);#cite(<Wu_et_al_2022>);.

Nevertheless, is not the classification of misfits as outliers that raises concerns for trait measurement and inference. Rather, the concern lies in the prevailing CJ practice of identifying these observations through ad-hoc procedures and excluding them from analysis #cite(<Pollitt_2012a>);#cite(<Pollitt_2012b>);, often without empirical support for the various hypotheses suggested to explain their occurrence. It is essential to recognize that outliers are defined relative to a specific model #cite(<McElreath_2020>);. Thus, detection procedures based on models like the BTL–which rests on strong assumptions–should be approached with caution, as they may not be appropriate for this purpose #cite(<Kelly_et_al_2022>);. If the BTL model does not accurately represent the actual data-generating process of the CJ system, the method may miss classify valid observations as misfits. On top this, excluding these observations carries additional risks. The statistical literature cautions that removing outliers can discard valuable information #cite(<Miller_2023>) and introduce bias into trait estimates. The direction and magnitude of this bias are often unpredictable, as they depend on which observations are excluded from the analysis #cite(<Zimmerman_1994>);#cite(<OHagan_2018>);#cite(<McElreath_2020>);. Finally, even when the model aligns well with the data, its rigid assumptions limit the model’s ability to adequately test many of the hypotheses proposed to explain misfit behavior.

In contrast, the approach presented in this study provides a rigorous framework for testing several relevant hypotheses. For instance, it allows to investigate whether misfit judges are those who exhibit \(an outlying degree of) systematic bias or greater variability in their judgments compared to their peers. Similarly, the approach makes possible to explore whether misfit stimuli exhibit more variable discriminal processes relative to other stimuli or if they are genuinely outlying cases. Moreover, since outliers are not inherently "bad data" #cite(<McElreath_2020>);, the present approach offers a principled alternative to exclusion, one that retains misfits in the analysis without compromising trait estimation or inference. This is achieved by adapting the proposed model into robust measurement models #cite(<McElreath_2020>);, a broad class of procedures designed to reduce the sensitivity of parameter estimates to mild or moderate departures from model assumptions #cite(<Everitt_et_al_2010>);. This strategy also speaks to a broader concern in the social sciences: when predictive power is low, the solution may not lie solely in seeking new variables or procedures, but also in adopting more sophisticated measurement models #cite(<Wainer_et_al_1978>);.

== Study limitations and practical challenges for applied CJ researchers
<sec-discussion_challenges>
Drawing conclusions from observed data always requires assumptions, whether the data are observational or experimental #cite(<Kohler_et_al_2019>);#cite(<Deffner_et_al_2022>);. The proposed approach is not an exception to this fundamental principle. Like all approaches grounded on causal inference, it relies on expert knowledge and assumptions about the variables’ causal structure that are often untestable at the outset #cite(<Hernan_et_al_2025>);. However, its purpose is not to deliver automatic answers when applied to a given CJ dataset. Instead, it encourages the formulation of precise questions and the explicit articulation of assumptions, fostering a generalizable understanding of the CJ system under study #cite(<Rohrer_et_al_2022>);#cite(<Deffner_et_al_2022>);#cite(<Sterner_et_al_2024>);. This clarity is crucial because the accuracy of estimates and validity of inferences depend on how well the data and inferential goals align with a model’s assumptions #cite(<Kohler_et_al_2019>);. Although this alignment remains to be empirically tested for the proposed models, the theory-driven nature of this approach provides a solid foundation for future empirical evaluation of its causal assumptions #cite(<Deffner_et_al_2022>);, grounded in both established theory and existing evidence.

The theoretical commitment to causal inference also introduces several practical challenges that applied CJ researchers must navigate. These fall into two main categories: first, acquiring the foundational knowledge necessary to apply the approach effectively; and second, dedicating greater attention to both conceptual and statistical modeling.

=== Required foundational knowledge
<sec-discussion_challenges1>
Applying the approach effectively requires foundational knowledge in two areas. First, a solid understanding of causal inference principles. Second, the ability to translate the functional and probabilistic aspects of conceptual models into bespoke statistical models. A clear example illustrating the importance of causal inference is the recurrent assumption that predictor variables are "relevant" to the research context–interpreting this relevance as their inclusion in a #emph[sufficient adjustment set] #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Morgan_et_al_2014>);. However, this study does not explore the full implications of that assumption for model specification, estimation, and inference. Nevertheless, to assist with the effort of engaging with this and other complex causal inference concepts, key references are provided throughout the study to guide applied CJ researchers toward a deeper understanding of these ideas #footnote[Also refer to the detailed online document referenced in the Declarations section];.

Developing the skills to translate conceptual models into bespoke statistical models also presents challenges. Although Bayesian inference methods offer a more accessible path for many applied CJ researchers to develop these skills–by reducing the need for specialized knowledge in areas like optimization theory, which frequentist methods often require–they still demand familiarity with technical concepts such as probabilistic programming languages \(PPLs), probability distributions, and convergence diagnostics. Thus, to support this skill development, this study provides a link to the statistical model code and alternative model specifications in the #strong[Declarations] section of this document #footnote[Seminal texts on Bayesian inference methods, such as #cite(<Gelman_et_al_2014>) and #cite(<McElreath_2020>);, offer valuable support for developing a deeper understanding of these models.];.

=== Attention to conceptual and statistical modeling
<sec-discussion_challenges2>
Even after acquiring the necessary foundational knowledge, applying this approach to a specific CJ context involves two additional challenges. First, it is essential to verify whether the conceptual model offers a #emph[faithful] #footnote[Avoid confusing this term with the #emph[faithfulness assumption] #cite(<Neal_2020>);#cite(<Hernan_et_al_2025>) described in the causal inference literature. For further details, see footnote 1.] representation of the CJ system under study. Second, the statistical translation of the model must be assessed to determine if it can accurately estimate the intended estimands \(i.e., parameters) from empirical data. To ensure fidelity, the conceptual and statistical models presented here should be treated as starting points, not universal solutions for all CJ designs or datasets. While these models may be adequate in some contexts, assuming so without evaluation is unwise. Instead, adaptation to the specific context is necessary, with careful attention to assessment design features and causal assumptions. As discussed in @sec-discussion_challenges, this approach supports such adaptation by providing a transparent framework for articulating new assumptions and guiding CJ assessment design.

Conversely, evaluating the estimation capabilities of the statistical model requires addressing the challenge of identification analysis. As outlined in @sec-theoretical, #emph[identification analysis] determines whether a statistical model can accurately compute a given estimand \(e.g., a parameter) based solely on its \(causal) assumptions, independent of random variability #cite(<Schuessler_et_al_2023>);. Identification is crucial because it is a necessary condition for consistency. #emph[Consistency] is the property of an estimator \(e.g., a statistical model) whose estimates converge to the "true" value of an estimand as the data size approaches infinity #cite(<Everitt_et_al_2010>);. Without identification, consistency is impossible–even with infinite, error-free data–and meaningful inference from finite samples cannot be achieved #cite(<Schuessler_et_al_2023>);. While formal derivations of identification may seem a natural next step, the complexity of the CJ system makes this approach impractical at the outset. Instead, simulation-based methods like power analysis offer a more practical and flexible alternative, enabling examination of estimate consistency without relying on complex mathematical proofs. Notably, the approach presented here supports both strategies by providing the probabilistic foundation for formal derivations and the statistical structure needed for simulation-based methods.

= Conclusion
<sec-conclusion>
The present study highlights the need to extend Thurstone’s theory to address the demands of contemporary empirical CJ research. It advocates for developing bespoke CJ models tailored to the specific data-generating processes of different CJ assessment designs. These models aim to enhance the robustness of trait estimates and clarity of inferences. Moreover, this work outlines a clear path for advancing both theoretical and applied CJ research and lays the foundation for broader adoption of more robust and interpretable methodologies across the social sciences.

#pagebreak()
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
[
Declarations
]
)
]
#strong[Funding:] The Research Fund \(BOF) of the University of Antwerp funded this project.

#strong[Financial interests:] The authors declare no relevant financial interests.

#strong[Non-financial interests:] The authors declare no relevant non-financial interests.

#strong[Ethics approval:] The University of Antwerp Research Ethics Committee confirmed that this study does not require ethical approval.

#strong[Consent to participate:] Not applicable

#strong[Consent for publication:] All authors have read and approved the final version of the manuscript for publication.

#strong[Data availability:] This study did not use any data.

#strong[Materials and code availability:] The `CODE LINK` section at the top of the digital document located at: #link("https://jriveraespejo.github.io/paper2_manuscript/") provides access to all materials and code.

#strong[AI-assisted technologies in the writing process:] The authors used various AI-based language tools to refine phrasing, optimize wording, and enhance clarity and coherence throughout the manuscript. They take full responsibility for the final content of the publication.

#strong[CRediT authorship contribution statement:] #emph[Conceptualization:] J.M.R.E, T.vD., S.DM., and S.G.; #emph[Methodology:] J.M.R.E, T.vD., and S.DM.; #emph[Software:] J.M.R.E.; #emph[Validation:] J.M.R.E.; #emph[Formal Analysis:] J.M.R.E.; #emph[Investigation:] J.M.R.E; #emph[Resources:] T.vD. and S.DM.; #emph[Data curation:] J.M.R.E.; #emph[Writing - original draft:] J.M.R.E.; #emph[Writing - review and editing:] T.vD., S.DM., and S.G.; #emph[Visualization:] J.M.R.E.; #emph[Supervision:] S.G. and S.DM.; #emph[Project administration:] S.G. and S.DM.; #emph[Funding acquisition:] S.G. and S.DM.

#pagebreak()
= Appendix
<sec-appendix>
== Statistical and Causal inference
<sec-appendixB>
This section introduces fundamental statistical and causal inference concepts necessary for understanding the core theoretical principles described in this document. It does not, however, offer a comprehensive overview of causal inference methods. Readers seeking more in-depth understanding may wish to explore introductory papers such as #cite(<Pearl_2010>);, #cite(<Rohrer_2018>);, #cite(<Pearl_2019>);, and #cite(<Cinelli_et_al_2020>);. They may also find it helpful to consult introductory books like #cite(<Pearl_et_al_2018>);, #cite(<Neal_2020>);, and #cite(<McElreath_2020>);. For more advanced study, readers may refer to seminal intermediate papers such as #cite(<Neyman_et_al_1923>);, #cite(<Rubin_1974>);, #cite(<Spirtes_et_al_1991>);, and #cite(<Sekhon_2009>);, as well as books such as #cite(<Pearl_2009>);, #cite(<Morgan_et_al_2014>);, and #cite(<Hernan_et_al_2025>);.

=== Empirical research and randomized experiments
<sec-appendixB1>
Empirical research uses evidence from observation and experimentation to address real-world challenges. In this context, researchers typically formulate their research questions as #emph[estimands] or #emph[targets of inference];, i.e., the specific quantities they seek to determine #cite(<Everitt_et_al_2010>);. For instance, researchers might be interested in answering the following question: "To what extent do different teaching methods $(T)$ influence students’ ability to produce high-quality written texts $(Y)$?" To investigate this, researchers could randomly assign students to two groups, each exposed to a different teaching method $(T_i = { 1 , 2 })$. Then, they would perform pairwise comparisons, generating a dichotomous outcome $(Y_i = { 0 , 1 })$ showing which student exhibits more of the ability. In this scenario, the research question can be rephrased as the estimand, "#emph[On average];, is there a difference in the ability to produce high-quality written texts between the two groups of students?" and this estimand can be mathematically represented by the random associational quantity in @eq-group_diff, where $E [dot.op]$ denotes the expected value.

#math.equation(block: true, numbering: "(1)", [ $ E [Y_i divides T_i = 1] - E [Y_i divides T_i = 2] $ ])<eq-group_diff>

Researchers then proceed to identify the estimands. #emph[Identification] determines whether an estimator can accurately compute the estimand based solely on its assumptions, regardless of random variability #cite(<Schuessler_et_al_2023>)[, p.~4];. An #emph[estimator] refers to a method or function that transforms data into an estimate #cite(<Neal_2020>);. #emph[Estimates] are numerical values that approximate the estimand derived through the process of #emph[estimation];, which integrates data with an estimator #cite(<Everitt_et_al_2010>);. The Identification-Estimation flowchart #cite(<McElreath_2020>);#cite(<Neal_2020>);, shown in @fig-IEflow, visually represents the transition from estimands to estimates.

#figure([
#box(width: 35%,image("images/png/IEflow.png"))
], caption: figure.caption(
position: bottom, 
[
Identification-Estimation flowchart. Extracted and slightly modified from #cite(<Neal_2020>)[p.~32]
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-IEflow>


Identification is a necessary condition to ensure #emph[consistent] estimators. An estimator achieves #emph[consistency] when it converges to the "true" value of an estimand as the data size approaches infinity #cite(<Everitt_et_al_2010>);. Without identification, researchers cannot achieve consistency, even with "infinite" and error-free data. As a result, deriving meaningful insights about an estimand from finite data becomes impossible #cite(<Schuessler_et_al_2023>)[, p.~5];. Therefore, to ensure accurate and reliable estimates, researchers prioritize estimators with desirable identification properties. For instance, the Z-test is a widely used estimator for comparing group proportions, yielding accurate estimates when its underlying assumptions are satisfied #cite(<Kanji_2006>);. Furthermore, researchers can interpret estimates from the Z-test as causal, provided the data is collected through a randomized experiment.

Randomized experiments are widely recognized as the gold standard in evidence-based science #cite(<Hariton_et_al_2018>);#cite(<Hansson_2014>);. This recognition stems from their ability to enable researchers interpret associational estimates as causal. They achieve this by ensuring data, and by extension an estimator, satisfies several key identification properties, such as common support, no interference, and consistency #cite(<Morgan_et_al_2014>);#cite(<Neal_2020>);. The most critical property, however, is the elimination of confounding. #emph[Confounding] occurs when an external variable $X$ simultaneously influences the outcome $Y$ and the variable of interest $T$, resulting in spurious associations #cite(<Everitt_et_al_2010>);. Randomization addresses this issue by decoupling the association between the intervention allocation $T$ from any other variable $X$ #cite(<Morgan_et_al_2014>);#cite(<Neal_2020>);.

Nevertheless, researchers often face constraints that limit their ability to conduct randomized experiments. These constraints include ethical concerns, such as the assignment of individuals to potentially harmful interventions, and practical limitations, such as the infeasibility of, for example, assigning individuals to genetic modifications or physical impairments #cite(<Neal_2020>);. In these cases, causal inference offers a valuable alternative for generating causal estimates and understanding the mechanisms underlying specific data. In addition, the framework can provide significant theoretical insights that can enhance the design of experimental and observational studies #cite(<McElreath_2020>);.

=== Identification under causal inference
<sec-appendixB2>
Unlike classical statistical modeling, which focuses primarily on summarizing data and inferring associations, the #emph[causal inference] framework is designed to identify causes and estimate their effects using data #cite(<Shaughnessy_et_al_2010>);#cite(<Neal_2020>);. The framework uses rigorous mathematical techniques to address the #emph[fundamental problem of causality] #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Morgan_et_al_2014>);. This problem revolves around the question, "What would have happened 'in the world' under different circumstances?" This question introduces the concept of counterfactuals, which are instrumental in defining and identifying causal effects.

#emph[Counterfactuals] are hypothetical scenarios that are #emph[contrary to fact];, where alternative outcomes resulting from a given cause are neither observed nor observable #cite(<Neal_2020>);#cite(<Counterfactual_2024>);. The structural approach to causal inference #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>) provides a formal framework for defining counterfactuals. For instance, in the scenario described in @sec-appendixB1, the approach begins by defining the #emph[individual causal effect] \(ICE) as the difference between each student’s potential outcomes, as in @eq-ICE.

#math.equation(block: true, numbering: "(1)", [ $ tau_i = Y_i divides d o (T_i = 1) - Y_i divides d o (T_i = 2) $ ])<eq-ICE>

where $d o (T_i = t)$ represents the intervention operator, $Y_i divides d o (T_i = 1)$ represents the potential outcome under intervention $T_i = 1$, and $Y_i divides d o (T_i = 1)$ represents the potential outcome under intervention $T_i = 2$. Here, an #emph[intervention] involves assigning a constant value to the treatment variable for each student’s potential outcomes. Note that if a student is assigned to intervention $T_i = 1$, the potential outcome under $T_i = 2$ becomes a counterfactual, as it is no longer observed nor observable. To address this challenge, the structural approach extends the ICE to the #emph[average causal effect] \(ACE, @eq-ACE), representing the average difference between the students’ observed potential outcomes and their counterfactual counterparts.

#math.equation(block: true, numbering: "(1)", [ $ tau & = E [tau_i]\
 & = E [Y_i divides d o (T_i = 1)] - E [Y_i divides d o (T_i = 2)] $ ])<eq-ACE>

Even though counterfactuals are unobservable, researchers can still identify the ACE from associational estimates by leveraging the structural approach. The approach identifies the ACE by statistically conditioning data on a #emph[sufficient adjustment set] of variables $X$ #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Morgan_et_al_2014>);. This #emph[sufficient] set \(potentially empty) must block all non-causal paths between $T$ to $Y$ without opening new ones. When such a set exists, then $T$ and $Y$ are #emph[d-separated] by $X$ \($T med tack.t med Y divides X$) #cite(<Pearl_2009>);, and $X$ satisfies the #emph[backdoor criterion] #cite(<Neal_2020>)[, pp 37];. Here, #emph[conditioning] describes the process of restricting the focus to the subset of the population defined by the conditioning variable #cite(<Neal_2020>)[, p.~32] \(see @eq-CACE).

Conditioning on a sufficient adjustment set enables researchers to estimate the ACE, even when the data comes from an observational study. This process is feasible because such conditioning ensures that the ACE estimator satisfies several critical properties, including confounding elimination #cite(<Morgan_et_al_2014>);. Naturally, the validity of claims about the causal effects of $T$ on $Y$ now hinges on the assumption that $X$ serves as a sufficient adjustment set. However, as #cite(<Kohler_et_al_2019>)[p.~150] noted, drawing conclusions about the real world from observed data inevitably requires assumptions. This requirement holds true for both observational and experimental data.

For instance, if researchers cannot conduct the randomized experiments described in @sec-appendixB1 and must instead rely on observational data, they can still identify the ACE as long as an observed variable $X$, such as the socio-economic status of the school, satisfies the backdoor criterion. Under these circumstances, researchers first identify the #emph[conditional average causal effect] \(CACE, @eq-CACE)

#math.equation(block: true, numbering: "(1)", [ $ C A C E_t = E [Y_i divides T_i = t , X] $ ])<eq-CACE>

From the CACE, researchers can identify the ACE from associational quantities as in @eq-mACE1. This identification process is commonly known as the #emph[backdoor adjustment];. Here, $E_X [dot.op]$ represents the marginal expected value over $X$ #cite(<Morgan_et_al_2014>);.

#math.equation(block: true, numbering: "(1)", [ $ tau & = E [Y_i divides d o (T_i = 1)] - E [Y_i divides d o (T_i = 2)]\
 & = E_X [C A C E_1 - C A C E_2]\
 & = E_X [E [Y_i divides T_i = 1 , X] - E [Y_i divides T_i = 2 , X]] $ ])<eq-mACE1>

Notably, the approach extends the ACE identification for a continuous variable $T$ as in @eq-mACE_cont, ensuring broad applicability across different causal scenarios #cite(<Neal_2020>)[, p.~45]

#math.equation(block: true, numbering: "(1)", [ $ tau & = E [Y_i divides d o (T_i = t)]\
 & = d E_X [E [Y_i divides T_i = t , X]] \/ d t $ ])<eq-mACE_cont>

=== Diving into the specifics
<sec-appendixB3>
The structural approach to causal inference uses SCMs and DAGs to formally and graphically represent the presumed causal structure underlying the ACE #cite(<Pearl_2009>);#cite(<Pearl_et_al_2016>);#cite(<Gross_et_al_2018>);#cite(<Neal_2020>);. Essentially, these tools serve as #emph[conceptual \(theoretical) models] on which identification analysis rests #cite(<Schuessler_et_al_2023>)[, p.~4];. Thus, using these tools, researchers can determine which statistical models can identify \(ACE, CACE, or other), assuming the depicted causal structure is correct #cite(<McElreath_2020>);, thus enabling valid causal inference. @fig-IEflow shows the role of theoretical models in the inference process.

SCMs and DAGs support identification analysis through two key advantages. First, regardless of complexity, they can represent various causal structures using only five fundamental building blocks #cite(<Neal_2020>);#cite(<McElreath_2020>);. This feature allows researchers to decompose complex structures into manageable components, facilitating their analysis #cite(<McElreath_2020>);. Second, they depict causal relationships in a non-parametric and fully interactive way. This flexibility enables feasible ACE identification strategies without defining the variables’ data types, the functional form between them, or their parameters #cite(<Pearl_et_al_2016>)[, p.~35];.

Thus, @sec-appendixB31 and @sec-appendixB32 elaborate on the first advantage, while @sec-appendixB32 and @sec-appendixB33 do so for the second. Finally, @sec-appendixB34 explains how researchers use SCMs and DAGs alongside Bayesian inference methods in the estimation process.

==== The five fundamental block for SCMs and DAGs
<sec-appendixB31>
Figures @fig-dags_scms1, @fig-dags_scms2, @fig-dags_scms3, @fig-dags_scms4, and @fig-dags_scms5 display the five fundamental building blocks for SCMs and DAGs. The left panels of the figures show the formal mathematical models, represented by the SCMs, defined in terms of a set of #emph[endogenous] variables $V = { X_1 , X_2 , X_3 }$, a set of #emph[exogenous] variables $E = { e_(X 1) , e_(X 2) , e_(X 3) }$, and a set of functions $F = { f_(X 1) , f_(X 2) , f_(X 3) }$ #cite(<Pearl_2009>);#cite(<Cinelli_et_al_2020>);. Endogenous variables are those whose causal mechanisms a researcher chooses to model #cite(<Neal_2020>);. In contrast, exogenous variables represent #emph[errors] or #emph[disturbances] arising from omitted factors that the investigator chooses not to model explicitly #cite(<Pearl_2009>)[, p.~27,68];. Lastly, the functions, referred to as #emph[structural equations];, express the endogenous variables as non-parametric functions of other variables. These functions use the symbol '$:=$' to denote the asymmetrical causal dependence of the variables and the symbol '$med tack.t med$' to represent #emph[d-separation];, a concept akin to \(conditional) independence.

Notably, every SCM has an associated DAG #cite(<Pearl_et_al_2016>);#cite(<Cinelli_et_al_2020>);. The right panels of the figures display these DAGs. A DAG is a graph consisting of nodes connected by edges, where the nodes represent random variables. The term #emph[directed] means that the edges extend from one node to another, with arrows indicating the direction of causal influence. The term #emph[acyclic] implies that the causal influences do not form loops, ensuring the influences do not cycle back on themselves #cite(<McElreath_2020>);. DAGs represent observed variables as solid black circles, while they use open circles for unobserved \(latent) variables #cite(<Morgan_et_al_2014>);. Although the #emph[standard representation] of DAGs typically omits exogenous variables for simplicity, the #emph[magnified representation] depicted in the figures offers one key advantage: including exogenous variables can help researchers highlight potential issues related to conditioning and confounding #cite(<Cinelli_et_al_2020>);.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X_1 & := f_(X 1) (e_(X 1))\
X_3 & := f_(X 3) (e_(X 3))\
e_(X 1) & med tack.t med e_(X 3) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_bb1>


]
,
  [
#figure([
#box(width: 54%,image("images/png/mdag_bb1.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_bb1>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Two unconnected nodes
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dags_scms1>


#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X_1 & := f_(X 1) (e_(X 1))\
X_3 & := f_(X 3) (X_1 , e_(X 3))\
e_(X 1) & med tack.t med e_(X 3) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_bb2>


]
,
  [
#figure([
#box(width: 54%,image("images/png/mdag_bb2.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_bb2>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Two connected nodes or descendant
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dags_scms2>


#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X_1 & := f_(X 1) (e_(X 1))\
X_2 & := f_(X 2) (X_1 , e_(X 2))\
X_3 & := f_(X 3) (X_2 , e_(X 3))\
e_(X 1) & med tack.t med e_(X 2)\
e_(X 1) & med tack.t med e_(X 3)\
e_(X 2) & med tack.t med e_(X 3) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_bb3>


]
,
  [
#figure([
#box(width: 65%,image("images/png/mdag_bb3.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_bb3>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Chain or mediator
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dags_scms3>


#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X_1 & := f_(X 1) (X_2 , e_(X 1))\
X_2 & := f_(X 2) (e_(X 2))\
X_3 & := f_(X 3) (X_2 , e_(X 3))\
e_(X 1) & med tack.t med e_(X 2)\
e_(X 1) & med tack.t med e_(X 3)\
e_(X 2) & med tack.t med e_(X 3) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_bb4>


]
,
  [
#figure([
#box(width: 65%,image("images/png/mdag_bb4.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_bb4>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Fork or confounder
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dags_scms4>


#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X_1 & := f_(X 1) (e_(X 1))\
X_2 & := f_(X 2) (X_1 , X_3 , e_(X 2))\
X_3 & := f_(X 3) (e_(X 3))\
e_(X 1) & med tack.t med e_(X 2)\
e_(X 1) & med tack.t med e_(X 3)\
e_(X 2) & med tack.t med e_(X 3) $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_bb5>


]
,
  [
#figure([
#box(width: 65%,image("images/png/mdag_bb5.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_bb5>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Collider or inmorality
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dags_scms5>


A careful examination of these building blocks highlights the theoretical assumptions underlying their observed variables. SCM @fig-scm_bb1[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms1] and DAG @fig-mdag_bb1[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms1] depict two unconnected nodes, representing a scenario where variables $X_1$ and $X_3$ are independent or not causally related. SCM @fig-scm_bb2[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms2] and DAG @fig-mdag_bb2[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms2] illustrate two connected nodes, representing a scenario where a #emph[parent] node $X_1$ exerts a causal influence on a #emph[child] node $X_3$. In this setup, $X_3$ is considered a #emph[descendant] of $X_1$. Additionally, $X_1$ and $X_3$ are described as #emph[adjacent] because there is a #emph[direct path] connecting them. SCM @fig-scm_bb3[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms3] and DAG @fig-mdag_bb3[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms3] depict a #emph[chain];, where $X_1$ influences $X_2$, and $X_2$ influences $X_3$. In this configuration, $X_1$ is a parent node of $X_2$, which is a parent node of $X_3$. This structure creates a #emph[directed path] between $X_1$ and $X_3$. Consequently, $X_1$ is an #emph[ancestor] of $X_3$, and $X_2$ fully #emph[mediates] the relationship between the two. SCM @fig-scm_bb4[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms4] and DAG @fig-mdag_bb4[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms4] illustrate a #emph[fork];, where variables $X_1$ and $X_3$ are both influenced by $X_2$. Here, $X_2$ is a parent node that #emph[confounds] the relationship between $X_1$ and $X_3$. Finally, SCM @fig-scm_bb5[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms5] and DAG @fig-mdag_bb5[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms5] show a #emph[collider];, where variables $X_1$ and $X_3$ are concurrent causes of $X_2$. In this configuration, $X_1$ and $X_3$ are not causally related to each other but both influence $X_2$ \(an #emph[inmorality];). Notably, all building blocks assume the errors are independent of each other and from all other variables in the graph, as evidenced by the pairwise relations $e_(X 1) med tack.t med e_(X 2)$, $e_(X 1) med tack.t med e_(X 3)$, and $e_(X 2) med tack.t med e_(X 3)$.

Researchers can then use these building blocks to represent the scenario outlined in @sec-appendixB2. SCM @fig-scm_example1[45127368-afa1-446a-820f-fc64c546b2c5%fig-example1] and DAG @fig-mdag_example1[45127368-afa1-446a-820f-fc64c546b2c5%fig-example1] depict the plausible causal structure for this example. In this context, the variable $X$ \(socio-economic status of the school) is thought to be a confounder in the relationship between the teaching method $T$ and the outcome $Y$. The figures display multiple descendant relationships such as $X arrow.r T$, $X arrow.r Y$, and $T arrow.r Y$. They also highlight unconnected node pairs, evident from the relationships $e_T med tack.t med e_X$, $e_T med tack.t med e_Y$, and $e_X med tack.t med e_Y$. Additional, the figures show one fork, $X arrow.r { T , Y }$, and two colliders: ${ X , e_T } arrow.r T$ and ${ X , T , e_Y } arrow.r Y$.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X & := f_X (e_X)\
T & := f_T (X , e_T)\
Y & := f_Y (T , X , e_Y)\
e_T & med tack.t med e_X\
e_T & med tack.t med e_Y\
e_X & med tack.t med e_Y $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_example1>


]
,
  [
#figure([
#box(width: 65%,image("images/png/mdag_example1.png"))
], caption: figure.caption(
position: bottom, 
[
DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_example1>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Plausible causal structure the scenario outlined in @sec-appendixB2.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-example1>


==== The probabilistic implications of these blocks
<sec-appendixB32>
Beyond their graphical capabilities, researchers can translate SCMs and DAGs into statistical models because these conceptual models encode functional and probabilistic information, which they can then replace with suitable functions and probabilistic assumptions #cite(<Pearl_et_al_2016>);. Notably, this encoding relies on two fundamental assumptions: the causal Markov and the faithfulness assumption. The #emph[causal Markov assumption] links the graph structure to the joint probability distribution of variables by asserting that any variable in a graph is independent of all its non-descendants conditional on its parents #cite(<Neal_2020>)[, p.~20];#cite(<Hernan_et_al_2025>)[, p.~80];. The #emph[faithfulness assumption];, in turn, states that the observed statistical \(in)dependencies in the data reflect the underlying causal structure among variables–except in rare degenerate cases #cite(<Neal_2020>)[, p.~100];#cite(<Hernan_et_al_2025>)[, p.~81];.

A notable implication of the assumptions underlying the probabilistic encoding is that any conceptual model described by an SCM and DAG can represent the joint distribution of variables more efficiently #cite(<Pearl_et_al_2016>)[, p.~29];. This expression takes the form of a product of conditional probability distributions \(CPDs) of the type $P (c h i l d divides p a r e n t s)$. This property is formally known as the #emph[Bayesian Network factorization] \(BNF, @eq-net_factor) #cite(<Pearl_et_al_2016>)[, p.~29];#cite(<Neal_2020>)[, p.~21];. In this expression, $p a (X_i)$ denotes the set of variables that are the parents of $X_i$. #math.equation(block: true, numbering: "(1)", [ \$\$
\\begin{aligned}
P\(X\_{1}, X\_{2}, \\dots, X\_{P}) & \= X\_{1} \\cdot \\prod^{P}\_{p\=2} P\(X\_{i} \\mid X\_{i-1}, \\dots, X\_{1}) & \(\\small{\\text{by chain rule})}\\\\
& \= X\_{1} \\cdot \\prod^{P}\_{p\=2} P\(X\_{i} \\mid pa\(X\_{i}) ) & \(\\small{\\text{by BNF}})
\\end{aligned}
\$\$ ])<eq-net_factor>

This encoding enables researchers with conceptual \(theoretical) knowledge in the form of an SCM and DAG to predict patterns of \(in)dependencies in the data. As highlighted by #cite(<Pearl_et_al_2016>)[p.~35];, these predictions depend solely on the structure of these conceptual models without requiring the quantitative details of the equations or the distributions of the errors. Moreover, once researchers observe empirical data, the patterns of \(in)dependencies in the data can provide significant insights into the validity of the proposed conceptual model.

The five fundamental building blocks described in @sec-appendixB31 clearly illustrate which \(in)dependencies can SMCs and DAGs predict. For instance, applying the BNF to the causal structure shown in the SCM @fig-scm_bb1[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms1] and DAG @fig-mdag_bb1[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms1] enables researchers to express the joint probability distribution of the observed variables as $P (X_1 , X_3) = P (X_1) P (X_3)$, supporting the theoretical assumption that the observed variables $X_1$ and $X_3$ are unconditionally independent \($X_1 med tack.t med X_3$) #cite(<Neal_2020>)[, p.~24];. Conversely, when $X_3$ is unconditionally dependent on $X_1$ \(\$X\_{1} \\:\\not\\bot\\:X\_{3}\$), as depicted in the SCM @fig-scm_bb2[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms2] and DAG @fig-mdag_bb2[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms2], the BNF express their joint probability distribution as $P (X_1 , X_3) = P (X_3 divides X_1) P (X_1)$. Notably, these descriptions demonstrate the clear correspondence between the structural equations illustrated in @sec-appendixB31 and the CPDs.

Beyond the insights gained from two-node structures, researchers can uncover more nuanced patterns of\(in)dependencies from chains, forks, and colliders. These \(in)dependencies apply to any data set generated by a causal model with those structures, regardless of the specific functions attached to the SCM #cite(<Pearl_et_al_2016>)[, p.~36];. For instance, applying the BNF to the chain structure depicted in the SCM @fig-scm_bb3[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms3] and DAG @fig-mdag_bb3[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms3] allow researchers to represent the joint distribution for the observed variables as $P (X_1 , X_2 , X_3) =$ $P (X_1) P (X_2 divides X_1) P (X_3 divides X_2)$. This expression implies that $X_1$ and $X_3$ are unconditionally dependent \$\(X\_{1} \\:\\not\\bot\\:X\_{3})\$, but conditionally independent when controlling for $X_2$ $(X_1 med tack.t med X_3 divides X_2)$. Moreover, in the fork structure shown in the SCM @fig-scm_bb4[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms4] and DAG @fig-mdag_bb4[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms4], researchers can express the joint distribution of the observed variables as $P (X_1 , X_2 , X_3) =$ $P (X_1 divides X_2) P (X_2) P (X_3 divides X_2)$. Similar to the chain structure, this expression allows researchers to further infer that $X_1$ and $X_3$ are unconditionally dependent \$\(X\_{1} \\:\\not\\bot\\:X\_{3})\$, but conditionally independent when controlling for $X_2$ $(X_1 med tack.t med X_3 divides X_2)$. Finally, researchers analyzing the collider structure illustrated in the SCM @fig-scm_bb5[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms5] and DAG @fig-mdag_bb5[45127368-afa1-446a-820f-fc64c546b2c5%fig-dags_scms5] can express the joint distribution of the observed variables as $P (X_1 , X_2 , X_3) =$ $P (X_1) P (X_2 divides X_1 , X_3) P (X_3)$. This representation allows researchers to infer that $X_1$ and $X_3$ are unconditionally independent $(X_1 med tack.t med X_3)$, but conditionally dependent when controlling for $X_2$ \$\(X\_{1} \\:\\not\\bot\\:X\_{3} \\mid X\_{2})\$. The authors #cite(<Pearl_et_al_2016>)[p.~37, 40, 41] and #cite(<Neal_2020>)[p.~25–26] provide the mathematical proofs for these conclusions.

Using these additional probabilistic insights, researchers can revisit the scenario in @sec-appendixB2. In this context, applying the BNF to the SCM @fig-scm_example2[45127368-afa1-446a-820f-fc64c546b2c5%fig-example2] structure, enables the representation of the joint probability distribution of the observed variables as $P (Y , T , X) =$ $P (Y divides T , X) P (T divides X) P (X)$. From this expression, researchers can infer that the outcome $Y$ is unconditionally dependent on the teaching method $T$ \$\(Y \\:\\not\\bot\\:T)\$. This dependency arises from two key structures: a direct causal path from the teaching method $T$ to the outcome $Y$, represented by the two-connected-nodes structure $T arrow.r Y$ \(black path in DAG @fig-mdag_example2[45127368-afa1-446a-820f-fc64c546b2c5%fig-example2]), and a confounding non-causal path from the teaching method $T$ to the outcome $Y$ through the socio-economic status of the school $X$, represented by the fork structure $T arrow.l X arrow.r Y$ \(gray path in DAG @fig-mdag_example2[45127368-afa1-446a-820f-fc64c546b2c5%fig-example2]).

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X & := x\
T & := f_T (x , e_T)\
Y & := f_Y (T , x , e_Y)\
e_T & med tack.t med e_X\
e_T & med tack.t med e_Y\
e_X & med tack.t med e_Y $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_example2>


]
,
  [
#figure([
#box(width: 65%,image("images/png/mdag_example1a.png"))
], caption: figure.caption(
position: bottom, 
[
Conditioned DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_example2>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Plausible causal structure the scenario outlined in @sec-appendixB2.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-example2>


==== From probability to causality
<sec-appendixB33>
The structural approach to causal inference translates probabilistic insights into actionable strategies seeking to identify the ACE from associational quantities. The approach achieves this by relying on the #emph[modularity assumption];, which posits that intervening on a node alters only the causal mechanism of that node, leaving others unchanged #cite(<Neal_2020>)[, p.~34];.

The modularity assumption underpins the concepts of manipulated graphs and Truncated Factorization, which are essential for representing interventions $P (Y_i divides d o (T_i = t))$ within SCMs and DAGs. #emph[Manipulated graphs] simulate physical interventions by removing specific edges from a DAG, while preserving the remaining structure unchanged #cite(<Neal_2020>)[, p.~34];. In parallel, #emph[Truncated Factorization] \(TF) achieves a similar simulation by removing specific functions from the conceptual model and replacing them with constants, while keeping the rest of the structure unchanged #cite(<Pearl_2010>);. The probabilistic implications of this factorization are formalized in @eq-trunc_factor, where $S$ represents the subset of variables $X_p$ directly influenced by the intervention, while an example illustrating these concepts follows below.

#math.equation(block: true, numbering: "(1)", [ $ P (X_1 , X_2 , dots.h , X_P divides d o (S)) = cases(delim: "{", product P (X_p divides p a (X_p)) & upright("if") med p in.not S, 1 quad & upright("otherwise")) $ ])<eq-trunc_factor>

Using the TF, researchers can define the #emph[backdoor adjustment] to identify the ACE. This adjustment states that if a variable $X_p in S$ serves as a #emph[sufficient adjustment set] for the effect of $X_a$ on $X_b$, then the ACE can be identified using @eq-backdoor. The sufficient adjustment set \(potentially empty) must block all non-causal paths between $X_a$ and $X_b$ without introducing new paths. If such a set exists, then $X_a$ and $X_b$ are #emph[d-separated] by $X_p$ \($X_a med tack.t med X_b divides X_p$) #cite(<Pearl_2009>);, and $X_p$ satisfies the #emph[backdoor criterion] #cite(<Neal_2020>)[, p.~37];.

#math.equation(block: true, numbering: "(1)", [ $ P (X_a divides d o (X_b = x)) = sum_(X p) P (X_a divides X_b = x , X_p) P (X_p) $ ])<eq-backdoor>

Ultimately, the backdoor adjustment enables researchers to express the ACE as:

#math.equation(block: true, numbering: "(1)", [ $ tau & = E [X_a divides d o (X_b = 1)] - E [X_a divides d o (X_b = 2)]\
 & = E_(X p) [E [X_a divides d o (X_b = 1) , X_p] - E [X_a divides d o (X_b = 2) , X_p]]\
 & = sum_(X p) X_a dot.op P (X_a divides X_b = 1 , X_p) dot.op P (X_p) - sum_(X p) X_a dot.op P (X_a divides X_b = 2 , X_p) dot.op P (X_p) $ ])<eq-backdoor_adjustment>

With these new insights, researchers revisiting the scenario in @sec-appendixB32 can infer that the socio-economic status of the school, $X$, satisfies the backdoor criterion, assuming the causal structure depicted by the SCM @fig-scm_example2[45127368-afa1-446a-820f-fc64c546b2c5%fig-example2] and DAG @fig-mdag_example2[45127368-afa1-446a-820f-fc64c546b2c5%fig-example2] is correct. This means that $X$ serves as a sufficient adjustment set, as it effectively blocks all confounding non-causal paths introduced by the fork structure. Nevertheless, since $Y$ remains dependent on $T$ even after conditioning \$\(Y \\:\\not\\bot\\:T \\mid X)\$, this dependency can only be attributed to the direct causal effect $T arrow.r Y$. Notably, for the purpose of identification, the conditioned DAG @fig-mdag_example2[45127368-afa1-446a-820f-fc64c546b2c5%fig-example2] is equivalent to the manipulated DAG @fig-mdag_example3[45127368-afa1-446a-820f-fc64c546b2c5%fig-example3], because $X$ satisfies the backdoor criterion.

#figure([
#block[
#grid(columns: 2, gutter: 2em,
  [
#figure([
$ X & := f_X (e_X)\
T & := t\
Y & := f_Y (t , X , e_Y)\
e_T & med tack.t med e_X\
e_T & med tack.t med e_Y\
e_X & med tack.t med e_Y $

], caption: figure.caption(
position: bottom, 
[
SCM
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-scm_example3>


]
,
  [
#figure([
#box(width: 65%,image("images/png/mdag_example1b.png"))
], caption: figure.caption(
position: bottom, 
[
Manipulated DAG
]), 
kind: "quarto-subfloat-fig", 
supplement: "", 
numbering: "(a)", 
)
<fig-mdag_example3>


]
)
]
], caption: figure.caption(
position: bottom, 
[
Plausible causal structure the scenario outlined in @sec-appendixB32.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-example3>


Researchers can then apply the #emph[backdoor adjustment] to identify the ACE of $T$ on $Y$. They achieve this by first identifying the CACE of $T$ on $Y$ by conditioning on $X$, and then marginalizing this effect over $X$ to obtain the ACE. This process is expressed in @eq-mACE2 \(see @sec-appendixB2).

#math.equation(block: true, numbering: "(1)", [ $ tau & = E [Y_i divides d o (T_i = 1)] - E [Y_i divides d o (T_i = 2)]\
 & = E_X [E [Y_i divides T_i = 1 , X] - E [Y_i divides T_i = 2 , X]]\
 & = sum_X Y_i dot.op P (Y_i divides T_i = 1 , X) dot.op P (X) - sum_X Y_i dot.op P (Y_i divides T_i = 2 , X) dot.op P (X) $ ])<eq-mACE2>

==== The estimation process
<sec-appendixB34>
Ultimately, researchers can use Bayesian inference methods to estimate the ACE. The approach begins by defining two probability distributions: the likelihood of the data, $P (X_1 , X_2 , dots.h , X_P divides theta)$, and the prior distribution, $P (theta)$ #cite(<Everitt_et_al_2010>);, where $X_P$ represents a random variable, and $theta$ represents a one-dimensional parameter space for simplicity. After observing empirical data, researchers can update the priors to posterior distributions using Bayes’ rule in @eq-bayes_rule:

#math.equation(block: true, numbering: "(1)", [ $ P (theta divides X_1 , X_2 , dots.h , X_P) = frac(P (X_1 , X_2 , dots.h , X_P divides theta) dot.op P (theta), P (X_1 , X_2 , dots.h , X_P)) $ ])<eq-bayes_rule>

Given that the denominator on the right-hand side of @eq-bayes_rule serves as a normalizing constant independent of the parameter $theta$, researchers can simplify the posterior updating process into three steps. First, they integrate new empirical data through the likelihood. Second, they update the parameters’ priors to a posterior distribution according to @eq-prop_rule. Ultimately, they normalize these results to obtain a valid probability distribution.

#math.equation(block: true, numbering: "(1)", [ $ P (theta divides X_1 , X_2 , dots.h , X_P) prop P (X_1 , X_2 , dots.h , X_P divides theta) dot.op P (theta) $ ])<eq-prop_rule>

Temporarily setting aside the definition of prior distributions $P (theta)$, note that the posterior updating process depends heavily on the assumptions underlying the likelihood of the data. However, as the number of random variables, $P$, increases, this joint distribution quickly becomes intractable #cite(<Neal_2020>);. This intractability is evident from @eq-like_chain, where the likelihood distribution is expressed by multiple chained CPDs.

#math.equation(block: true, numbering: "(1)", [ $ P (X_1 , X_2 , dots.h , X_P divides theta) = P (X_1 divides theta) product_(p = 2)^P P (X_i divides X_(i - 1) , dots.h , X_1 , theta) $ ])<eq-like_chain>

Nevertheless, researchers can manage the complexity of the likelihood by assuming specific local \(in)dependencies among variables. SCMs and DAGs provide a formal framework to represent these assumptions, as detailed in @sec-appendixB32. These assumptions improve model tractability and simplify the estimation process by enabling the derivation of the BNF of the likelihood \(@eq-like_BNF). With this simplified structure, any probabilistic programming language can model the system and compute the parameter’s posterior distribution using @eq-bayes_rule.

#math.equation(block: true, numbering: "(1)", [ $ P (X_1 , X_2 , dots.h , X_P divides theta) = P (X_1 divides theta) product_(p = 2)^P P (X_i divides p a (X_i) , theta) $ ])<eq-like_BNF>

#pagebreak()
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
[
References
]
)
]
#block[
] <refs>



#set bibliography(style: "apa")

#bibliography("references.bib")

