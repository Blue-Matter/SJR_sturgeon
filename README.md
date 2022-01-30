# St John River (SJR) Atlantic sturgeon assessment

This repository hosts the model files for the 2021 SJR Atlantic sturgeon assessment:

- `01A_CSF_0.6BOF` is the closest thing to a 'base' assessment, as no single model was presented by itself. 
- `01A_CSF_0.6BOF_dome` implements dome selectivity for the SJR fishery.
- `03A_SSF_0.6BOF` makes an assumption about the sex ratio of historical catches prior to 2007.

To run these models, copy and paste `ss3.exe` into the corresponding folder. Run the model in the command line with: `ss.exe -nox` where
`-nox` reduces console output.

From these three models, the full suite of models presented in the assessment can be re-created, by adjusting the control file, performing a likelihood profile, etc.

The TMB file for the Brownie tagging analysis is also hosted here.
