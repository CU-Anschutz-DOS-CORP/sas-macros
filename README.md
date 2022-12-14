This site contains a collection of SAS macros created and/or maintained by the DOS-CORP team. Go <a href="https://cu-anschutz-dos-corp.github.io/sas-macros/files.html" target="_blank">here</a> to access the documentation as well as view the raw .SAS files.

## How to access a macro directly from GitHub

You can download and compile a single macro directly from GitHub as in the following example:
```sas
filename mc url "https://raw.githubusercontent.com/CU-Anschutz-DOS-CORP/sas-macros/main/summarize.sas";
%inc mc;
```
## How to access locally stored macros

You can also download the entire collection directly from our <a href="https://github.com/CU-Anschutz-DOS-CORP/sas-macros" target="_blank">GitHub repository</a> and save it in your computer and access these files from your SAS session.

### How to include a single macro in a SAS session

Use `%INC` before you call the macro of interest:
```sas
%include “path-to-local-repo/summarize.sas”;
```
### How to make all the macros in the repository available in a SAS session

Include the following code at the beginning of your program:
```sas
options 
	sasautos =
		(‘path-to-local-repo1’,
		 ‘path-to-local-repo2’,
		 sasautos)
	mautosource mrecall;
```
The SASAUTOS macro system option allows to specify the location of one or more autocall libraries. If the macro you are calling is not locally defined in the current session, SAS will open and search each location in the order provided.
