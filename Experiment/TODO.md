# TODO

## Coding experiment

- [x] download all the images
- [x] update consent form
- [x] replace HIT with submission and MTurk with Prolific
- [x] add debriefing at end
    > Please also include a short debriefing in your experiment, thanking the participant, explaining in 2-4 lines what your study was about, and asking them not to share this information with other potential participants.

- [x] replace the php data-saving with DataPipe after each contest
- [x] Code up your planned analyses
    1. jczimm/memo-sandbox has my planned modeling approach, just missing solitary and compensatory models as of 2025-11-4
    2. xiang2023/writeup/index.qmd has my planned analysis (the one-sample t-test)

- [x] test datapipe basically

## Pilot A

Pilot A is a pilot of your study with non-naive participants. The goals of Pilot A are:

- [x] Collect "data" from you and/or your friends to guarantee that you are logging data correctly.
- [x] Get feedback on the paradigm by running it several times.
- [x] Code up your planned analyses (i.e. implement data-preprocessing per my sketch) and confirm that you can run them on your data.

This assignment requires you to:

- [x] submit your rendered replication report with a link to your paradigm and with the limited data you collected analyzed via the confirmatory analyses.

## When launching on Prolific

- update payment rate from $2 to $3 to reflect Prolific requirement
    - [x] in index1.html
    - [x] in writeup/index.qmd
    - [ ] in qmd file attached to prereg
- [ ] implement changes to index1.html if needed to align with class guideline about whether we'll request return/reject ppl who fail both attention checks (see `termination` step, and elsewhere the string "reject" appears)

- [ ] Don't forget ?PROLIFIC_PID=... ! and maybe also do an auto-redirect instead of manual entry of completion code
- [ ] In the experiment, require prolificId to be supplied
- [ ] Enforce desktop-only?
- [ ] Actually add index.qmd so that going to ./xiang2023/ doesn't show participants the writeup (and ensure my link is always ..../xiang2023/writeup/ instead of just .../xiang2023/)
