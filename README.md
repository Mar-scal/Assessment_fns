# Assessment_fns

This is the core collection of functions used for the assessment of Offshore Scallop. aa

To install Git: https://git-scm.com/download/win 

Some tips/best practices: https://guides.github.com/introduction/flow/

We have modified the typical GitHub workflow so that we can work from our network and users can access their scripts from a remote VPN connection. Our general idea for a workflow comes from here https://nvie.com/files/Git-branching-model.pdf, with some revisions to work from a network and minimize uncessary duplication.

The workflow is as follows

1: New users should be added as collaborators (talk to Dave or Freya) 

2: FORK the Mar-scal MASTER to your personal github account
  - This is your personal DEVELOPMENT version of the MASTER repo housed in Mar-scal.

3: On your computer you will clone your FORK repo to Y:\Github\Offshore\Assessment_fns\YOUR_NAME.  
  - This directory is where you will work on revisions.
  - WE NEVER WORK in the directory where the MASTER resides (i.e. Y:\Offshore\Assessment\Assessment_fns)

4: All independent work is to be performed on a branch inside your personal FORK that resides in Y:\Github\....
  - Please name you branch something informative, e.g. Mapping update, survey bugfix, etc.

5: When you have completed your revisions COMMIT the revisions to your branch 

6: Create a PULL REQUEST to merge the data into your FORK. 
  - Within your fork you can take care of your own pull requests.  

7: Test your changes to ensure there are no bugs within your FORK

8: You can now submit a PULL REQUEST from your FORK to the Mar-scal MASTER 
  - For now DK/FK will review the PULL REQUEST to ensure it does not cause any issues.
      -DK/FK will also identify any ISSUES that need to be opened and/or closed, and integrated with pull requests too.
  - If you are aware of ISSUES being resolved/opened feel free to integrate these into the PULL REQUEST yourself

Collaborators *CAN* merge their pull requests independently, but complicated edits *MUST* be discussed before being merged with the master on Mar-scal as these are now immediately implemented on our shared network. 

