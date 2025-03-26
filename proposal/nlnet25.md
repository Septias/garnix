
### Abstract: Can you explain the **whole project** and its expected outcome(s) (1200c)

Nixos is a powerful, declarative, and reproducible operating system that leverages the Nix package manager. Even though it has a lot of great benefits, it still lacks general adoption, mainly because of its steep learning curve. One of the reasons for this steep learning curve is the custom nix language that implementors have to learn to use the package manager. Especially for newcomers it is hard to get behind the quirks of the nix language and its featurs. Being able to only check program correctness with the dynamic evaluator makes the process of writing nix programs time consuming and frustrating.


To improve the current situation, this project aims to enhance the Nix developer experience by expanding the [nil](https://github.com/oxalica/nil), a Language Server Protocol (LSP) for the nix language. The contributions I want to make are twofold, first, I want to implement a more sophisticated type inference algorithm to Nil, and second, I want to expand the LSP features that Nil currently supports. By doing so, I hope to provide Nix developers with a more robust and user-friendly experience, ultimately broadening adoption and strengthening the ecosystem.

### Have you been involved with projects or organisations relevant to this project before? And if so, can you tell us a bit about your contributions?(2500c)
I currently work at the Chair of Programming Languages at the University of Freiburg, where I have gained a strong foundation in programming languages, their theory, and practical implementations. In addition, I am a long-time contributor to the DeltaChat messenger app, where I have learned a lot about free and open-source software (FOSS) collaboration and best practices.

Beyond that, I have been an active user of NixOS for several years. I have successfully packaged two of my own projects for the Nix ecosystem and am currently in the process of packaging a third (DeltaChat Tauri). 


### Explain what the requested budget will be **used** for?
The total requested budget is **€30,000**, allocated exclusively for human labor at a rate of **€50/hour**. These funds will cover:

- **Design and Implementation**: Adapting and integrating the SimpleNix algorithm into Nil.  
- **Testing and Validation**: Thoroughly testing the enhanced type inference on real-world Nix projects.  
- **LSP Improvements**: Adding complementary features to Nil, improving editor integration and user experience.  
- **Community Engagement**: Documenting the updates, offering support, and collaborating with maintainers and users.


### Compare your own project with existing or historical efforts (4000c)
The Nix community has long discussed the need for better language tooling. Various initiatives have approached the problem from different angles:

- **Nixd**: Originated as a Rust-based extension to the Nix parser, offering autocompletion for Nix option fields. However, Nixd’s feature set remains limited, focusing on narrower aspects of the developer experience.  
- **Nil**: Already provides several functionalities, including option renaming, dead code analysis, and basic refactoring tools. However, it currently lacks robust type inference for more complex code structures
- **Nickel**: A standalone language that aims to replace Nix entirely, offering a more modern syntax and additional features. However, Nickel’s adoption would require a significant migration effort for existing Nix projects.

This project aims to enhance the existing Nix language by building on the Nil LSP server. This approach leverages the familiarity of the Nix language while providing advanced type inference capabilities and additional LSP features for new and old code alike. By focusing on incremental improvements to the existing ecosystem, this project aims to deliver tangible benefits without requiring a complete overhaul of existing workflows.


### What are significant technical challenges you expect to solve during the project, if any? (5000c)
This project can be broken down into three major phases, each posing its own technical challenges:

1. **Integrating the New Type Inference Algorithm**  
   - Adapting the current SimpleSub implementation to Nil’s backend structures.  
   - Managing complexity introduced by *salsa* (a crate for incremental and cache-friendly computations).  
   - Ensuring seamless interoperability within Nil’s existing architecture.
2. **Real-World Validation**  
   - Testing the enhanced type inference against large-scale, real-world Nix codebases.
   - Identifying edge cases and refining the algorithm accordingly.  
   - Ensuring the tool remains performant and stable under typical (and atypical) developer workflows.
3. **General Improvements to Nil**  
   - Enhancing overall user experience, including error reporting, code navigation, and other LSP features.
   - Maintaining modular design so improvements to Nil do not hinder further algorithmic optimizations. 
   - Building deeper familiarity with Nil’s internals to facilitate ongoing maintenance and future feature requests.


### Describe the ecosystem of the project, and how you will engage with relevant actors and promote the outcomes? (2500c)
Nix forms the backbone of the NixOS operating system and a standalone package manager that collectively hosts over 120,000 packages. Its emphasis on reliability, reproducibility, and security has attracted a growing audience of contributors and users, ranging from individual enthusiasts to large-scale system administrators.

Close collaboration with Nil’s maintainer, [@oxalica](https://github.com/oxalica), will ensure that enhancements are aligned with the larger Nix ecosystem roadmap. We will share progress and solicit feedback through established community channels (mailing lists, GitHub issues, forums) to drive adoption and create a feedback loop for continuous improvement. By bridging advanced language features with a well-supported LSP, this project strives to fortify Nix’s reputation for cutting-edge, user-friendly tooling and to empower both new and experienced Nix developers alike.
