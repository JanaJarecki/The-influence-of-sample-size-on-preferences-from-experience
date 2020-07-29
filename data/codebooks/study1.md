Codebook for data in Hoffart, Jarecki, Duthil & Rieskamp
================
Jana B. Jarecki
2020-07-29

This document contains codebooks explaining the variables and labels of
the data files study1.csv in the folder data/processed.

# Codebook

### Metadata

#### Description

**Dataset name**: study1

Choice data from the decision from experience experiment by Hoffart,
Jarecki, Duthil & Rieskamp

<details>

<summary title="Expand this section to see some additional metadata in a structured format that is useful for search engines">Metadata
for search engines</summary>

  - **Temporal Coverage**: NA

  - **Spatial Coverage**: NA

  - **Citation**: Hoffart, Jarecki, Duthil & Rieskamp

  - **Date published**: 2020-07-29

  - **Creator**:

| name        | value                                                                     |
| :---------- | :------------------------------------------------------------------------ |
| @type       | Person                                                                    |
| givenName   | Jana B.                                                                   |
| familyName  | Jarecki                                                                   |
| email       | <jj@janajarecki.com>                                                      |
| affiliation | list(`@type` = “Organization”, name = “University of Basel, Switzerland”) |

<table class="kable_wrapper">

<tbody>

<tr>

<td>

| x                                                                 |
| :---------------------------------------------------------------- |
| Experimental data with human participants, within-subjects design |

</td>

<td>

| x                                                   |
| :-------------------------------------------------- |
| Center for Economic Psychology, University of Basel |

</td>

<td>

| x                         |
| :------------------------ |
| online study              |
| risky choice              |
| risky gamble              |
| decisions from experience |
| psychology                |
| cognition                 |
| cognitive modeling        |
| heuristics                |
| bayesian cognitive model  |

</td>

</tr>

</tbody>

</table>

</details>

## Codebook table

![](study1_files/figure-gfm/cb_study1_items__items-1.png)<!-- -->

<script type="application/ld+json">
{
  "citation": "Hoffart, Jarecki, Duthil & Rieskamp",
  "creator": {
    "@type": "Person",
    "givenName": "Jana B.",
    "familyName": "Jarecki",
    "email": "jj@janajarecki.com",
    "affiliation": {
      "@type": "Organization",
      "name": "University of Basel, Switzerland"
    }
  },
  "spatialCoverage": "NA",
  "temporalCoverage": "NA",
  "measurementTechnique": "Experimental data with human participants, within-subjects design",
  "funder": "Center for Economic Psychology, University of Basel",
  "keywords": ["online study", "risky choice", "risky gamble", "decisions from experience", "psychology", "cognition", "cognitive modeling", "heuristics", "bayesian cognitive model"],
  "description": "Choice data from the decision from experience experiment by Hoffart, Jarecki, Duthil & Rieskamp\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name          |label                                                                       | n_missing|\n|:-------------|:---------------------------------------------------------------------------|---------:|\n|id            |Participant identifier                                                      |         0|\n|condition     |Within-subjects condition                                                   |         0|\n|trial         |Trial number for each participant                                           |         0|\n|gambleid      |Identifier of the gamble                                                    |         0|\n|gamblex       |Outcome of the risky gamble stimulus                                        |         0|\n|gamblep       |Probability of the risky gamble stimulus                                    |         0|\n|gambletype    |Type of gamble                                                              |         0|\n|samplesizecat |Level of sample size. Is '--' if condition is 'description'                 |         0|\n|samplesize    |Number of samples the participant drew. Is NA if condition is 'description' |       720|\n|value         |Respondents' evaluation response                                            |         0|\n|confidence    |Respondents self-reported confidence                                        |         0|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.9.2).",
  "name": "study1",
  "datePublished": "2020-07-29",
  "@context": "http://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "id",
      "description": "Participant identifier",
      "@type": "propertyValue"
    },
    {
      "name": "condition",
      "description": "Within-subjects condition",
      "value": "1. description,\n2. experience",
      "@type": "propertyValue"
    },
    {
      "name": "trial",
      "description": "Trial number for each participant",
      "@type": "propertyValue"
    },
    {
      "name": "gambleid",
      "description": "Identifier of the gamble",
      "value": "1. 1,\n2. 2,\n3. 3,\n4. 4,\n5. 5,\n6. 6",
      "@type": "propertyValue"
    },
    {
      "name": "gamblex",
      "description": "Outcome of the risky gamble stimulus",
      "@type": "propertyValue"
    },
    {
      "name": "gamblep",
      "description": "Probability of the risky gamble stimulus",
      "@type": "propertyValue"
    },
    {
      "name": "gambletype",
      "description": "Type of gamble",
      "value": "1. $-bet,\n2. p-bet",
      "@type": "propertyValue"
    },
    {
      "name": "samplesizecat",
      "description": "Level of sample size. Is '--' if condition is 'description'",
      "value": "1. --,\n2. l,\n3. m,\n4. s,\n5. xs",
      "@type": "propertyValue"
    },
    {
      "name": "samplesize",
      "description": "Number of samples the participant drew. Is NA if condition is 'description'",
      "@type": "propertyValue"
    },
    {
      "name": "value",
      "description": "Respondents' evaluation response",
      "@type": "propertyValue"
    },
    {
      "name": "confidence",
      "description": "Respondents self-reported confidence",
      "@type": "propertyValue"
    }
  ]
}
</script>

<details>

<summary>JSON-LD metadata</summary> The following JSON-LD can be found
by search engines, if you share this codebook publicly on the web.

``` json
{
  "citation": "Hoffart, Jarecki, Duthil & Rieskamp",
  "creator": {
    "@type": "Person",
    "givenName": "Jana B.",
    "familyName": "Jarecki",
    "email": "jj@janajarecki.com",
    "affiliation": {
      "@type": "Organization",
      "name": "University of Basel, Switzerland"
    }
  },
  "spatialCoverage": "NA",
  "temporalCoverage": "NA",
  "measurementTechnique": "Experimental data with human participants, within-subjects design",
  "funder": "Center for Economic Psychology, University of Basel",
  "keywords": ["online study", "risky choice", "risky gamble", "decisions from experience", "psychology", "cognition", "cognitive modeling", "heuristics", "bayesian cognitive model"],
  "description": "Choice data from the decision from experience experiment by Hoffart, Jarecki, Duthil & Rieskamp\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name          |label                                                                       | n_missing|\n|:-------------|:---------------------------------------------------------------------------|---------:|\n|id            |Participant identifier                                                      |         0|\n|condition     |Within-subjects condition                                                   |         0|\n|trial         |Trial number for each participant                                           |         0|\n|gambleid      |Identifier of the gamble                                                    |         0|\n|gamblex       |Outcome of the risky gamble stimulus                                        |         0|\n|gamblep       |Probability of the risky gamble stimulus                                    |         0|\n|gambletype    |Type of gamble                                                              |         0|\n|samplesizecat |Level of sample size. Is '--' if condition is 'description'                 |         0|\n|samplesize    |Number of samples the participant drew. Is NA if condition is 'description' |       720|\n|value         |Respondents' evaluation response                                            |         0|\n|confidence    |Respondents self-reported confidence                                        |         0|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.9.2).",
  "name": "study1",
  "datePublished": "2020-07-29",
  "@context": "http://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "id",
      "description": "Participant identifier",
      "@type": "propertyValue"
    },
    {
      "name": "condition",
      "description": "Within-subjects condition",
      "value": "1. description,\n2. experience",
      "@type": "propertyValue"
    },
    {
      "name": "trial",
      "description": "Trial number for each participant",
      "@type": "propertyValue"
    },
    {
      "name": "gambleid",
      "description": "Identifier of the gamble",
      "value": "1. 1,\n2. 2,\n3. 3,\n4. 4,\n5. 5,\n6. 6",
      "@type": "propertyValue"
    },
    {
      "name": "gamblex",
      "description": "Outcome of the risky gamble stimulus",
      "@type": "propertyValue"
    },
    {
      "name": "gamblep",
      "description": "Probability of the risky gamble stimulus",
      "@type": "propertyValue"
    },
    {
      "name": "gambletype",
      "description": "Type of gamble",
      "value": "1. $-bet,\n2. p-bet",
      "@type": "propertyValue"
    },
    {
      "name": "samplesizecat",
      "description": "Level of sample size. Is '--' if condition is 'description'",
      "value": "1. --,\n2. l,\n3. m,\n4. s,\n5. xs",
      "@type": "propertyValue"
    },
    {
      "name": "samplesize",
      "description": "Number of samples the participant drew. Is NA if condition is 'description'",
      "@type": "propertyValue"
    },
    {
      "name": "value",
      "description": "Respondents' evaluation response",
      "@type": "propertyValue"
    },
    {
      "name": "confidence",
      "description": "Respondents self-reported confidence",
      "@type": "propertyValue"
    }
  ]
}`
```

</details>
