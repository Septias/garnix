**Target:** <https://nlnet.nl/fediversity/>  
**Propose:** <https://nlnet.nl/propose/>

### Abstract: Can you explain the **whole project** and its expected outcome(s) (1200c)

This project aims to enhance the Nix developer experience by expanding [Nil](https://github.com/oxalica/nil), a Language Server Protocol (LSP) implementation for the Nix language. The contributions I want to make are twofold: first, I want to implement a more sophisticated type inference algorithm in Nil; second, I want to expand the LSP features that Nil currently supports. By doing so, I hope to provide Nix developers with a more robust and user-friendly experience, ultimately broadening adoption and strengthening the ecosystem.

One of NLnet’s goals is *fediversity*—the creation of a more diverse and decentralized internet. A foundational building block for decentralized and secure software is the NixOS operating system, which enables the creation of reproducible, secure, and reliable software. Despite its many benefits, it still lacks *general adoption*, mainly due to its steep learning curve. This is largely because of the Nix programming language that implementors must learn in addition to understanding the inner workings of an operating system—hence the reason for this proposal.

---

### Have you been involved with projects or organisations relevant to this project before? And if so, can you tell us a bit about your contributions? (2500c)

I currently work at the Chair of Programming Languages at the University of Freiburg, where I have gained a strong foundation in programming languages. My bachelor’s thesis explored the type inference options of the Nix programming language, and as part of that work, I implemented a simple type inference algorithm for Nix. As a continuation, I intend to align my master’s project with the theoretical foundations related to this proposal.

In addition, I am a long-time contributor to the DeltaChat messenger app, where I have learned a lot about free and open-source software (FOSS) collaboration and best practices. Beyond that, I have been an active user of NixOS for several years. I have successfully packaged two of my own projects for the Nix ecosystem and am currently in the process of packaging a third (DeltaChat Tauri).


### Explain what the requested budget will be **used** for?

The total requested budget is **€30,000**, allocated exclusively for human labor at a rate of **€50/hour**. These funds will cover:

- **Design and Implementation**: Adapting and integrating the SimpleNix algorithm into Nil.  
- **Testing and Validation**: Thoroughly testing the enhanced type inference on real-world Nix projects.  
- **LSP Improvements**: Adding complementary features to Nil, improving editor integration and user experience.  
- **Community Engagement**: Documenting the updates, offering support, and collaborating with maintainers and users.


### Compare your own project with existing or historical efforts (4000c)

The Nix community has long discussed the need for better language tooling. Various initiatives have approached the problem from different angles:

- **Nixd**: Originated as a Rust-based extension to the Nix parser, offering autocompletion for Nix option fields. However, Nixd’s feature set remains limited, focusing on narrower aspects of the developer experience.  
- **Nil**: Already provides several functionalities, including option renaming, dead code analysis, and basic refactoring tools. However, it currently lacks robust type inference for more complex code structures.  
- **Nickel**: A standalone language that aims to replace Nix entirely, offering a more modern syntax and additional features. However, Nickel’s adoption would require a significant migration effort for existing Nix projects.

This project aims to enhance the developer experience of the existing Nix language by building on the Nil LSP server. This approach leverages the familiarity of the Nix language while providing advanced type inference capabilities and additional LSP features for both new and existing code. By focusing on incremental improvements to the current ecosystem, this project aims to deliver tangible benefits without requiring a complete overhaul of existing workflows.


### What are significant technical challenges you expect to solve during the project, if any? (5000c)

This project can be broken down into three major phases, each posing its own technical challenges:

1. **Integrating the New Type Inference Algorithm**  
   - Adapting the current SimpleSub implementation to Nil’s backend structures.  
   - Managing complexity introduced by *salsa* (a crate for incremental and cache-friendly computations).  
   - Ensuring seamless interoperability within Nil’s existing architecture.

2. **Real-World Validation**  
   - Testing the enhanced type inference against large-scale, real-world Nix codebases.  
   - Identifying edge cases and refining the algorithm accordingly.  
   - Ensuring the tool remains performant and stable under common developer workflows.

3. **General Improvements to Nil**  
   - Enhancing overall user experience, including error reporting, code navigation, and other LSP features.  
   - Building deeper familiarity with Nil’s internals to facilitate ongoing maintenance and future feature requests.


### Describe the ecosystem of the project, and how you will engage with relevant actors and promote the outcomes? (2500c)

During this project, I aim to collaborate closely with the following stakeholders:

- **Nil’s maintainer**, [@oxalica](https://github.com/oxalica), whom I have contacted (though I have not yet received a response).  
- **The NixOS team**—I have not yet had the chance to connect with them, but I would be happy to get in touch.

