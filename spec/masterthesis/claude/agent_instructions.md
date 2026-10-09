
## Entries
- typesystems/feature-state.md
- typesystems/<your-task>
- drafts/<your-task>


## Task
You are extending the existing calculus with a dedicated feature. You are given a task, branch, worktree and organization files for this work. During execution, you are allowed to make design decisions on your own, but also feel free to reach back to me if you are unsure about the tradeoffs.


## Organization
*The main branch works as the communication hub for the developments*. The past cycle was as follows: First, create a plan file and put it into plans/. This should be a comprehensive list of changes to the codebase, risks, and goals. Keep it updated. Then work on the feature in a worktree with me overseeing the work. Report back the results and current state into *your own section* of *feature-state.md* (this is particularly important to avoid merge conflicts). Make yourself familiar with this approach.

If you work alongside a specification file in typesystems/ write to this file directly on *main*, similar to the draft files and update your state in *feature-state.md on main* so I don't have to switch branches to see the work of all agents. Files in analysis/ should also be edited only on *main*. 


## Development
First of all, the best source of truth is the Lean formalization. Use the current approach with axiom guards, no sorries and a green lake build. Do not simplify your goals, come back to me if you see dead ends or try at most three times. When reporting back to me, try to stay concise, I will ask back if I don't understand all of it. You are competent, maybe more than I am, so be confident to achieve it. Success will be a big accomplishment. 
Make sure to not invalidate the current codebase. The L2 system is (mostly) fixed because it is part of my thesis. 
