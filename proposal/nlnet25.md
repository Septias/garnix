**Target:** <https://nlnet.nl/fediversity/>  
**Propose:** <https://nlnet.nl/propose/>

### Abstract: Can you explain the whole project and its expected outcome(s) (1200c)

This project aims to enhance the Nix developer experience by expanding [Nil](https://github.com/oxalica/nil), a Language Server Protocol (LSP) implementation for the Nix language. The contributions I want to make are twofold: first, I want to implement a more sophisticated type inference algorithm in Nil; second, I want to expand the LSP features that Nil currently supports. By doing so, I hope to provide Nix developers with a more robust and user-friendly experience, ultimately broadening adoption and strengthening the ecosystem.

One of NLnet’s goals is fediversity — the creation of a more diverse and decentralized internet. A foundational building block for decentralized and secure software is the NixOS operating system, which enables the creation of reproducible, secure, and reliable software. Despite its many benefits, it still lacks general adoption, mainly due to its steep learning curve. This is largely because of the Nix programming language that implementors must learn in addition to understanding the inner workings of an operating system — hence the reason for this proposal.

### Have you been involved with projects or organisations relevant to this project before? And if so, can you tell us a bit about your contributions? (2500c)

I currently work at the Chair of Programming Languages at the University of Freiburg, where I have gained a strong foundation in programming languages. My bachelor’s thesis explored the type inference options of the Nix programming language, and as part of that work, I implemented a simple type inference algorithm for Nix. As a continuation, I intend to align my master’s project with the theoretical foundations related to this proposal.

In addition, I am a long-time contributor to the DeltaChat messenger app, where I have learned a lot about free and open-source software (FOSS) collaboration and best practices. Beyond that, I have been an active user of NixOS for several years. I have successfully packaged two of my own projects for the Nix ecosystem and am currently in the process of packaging a third (DeltaChat Tauri).


### Explain what the requested budget will be used for?

The total requested budget is €30,000, allocated exclusively for human labor at a rate of €50/hour. These funds will cover:

- Design and Implementation: Adapting and integrating the SimpleNix algorithm into Nil.
- Testing and Validation: Thoroughly testing the enhanced type inference on real-world Nix projects.
- LSP Improvements: Adding complementary features to Nil, improving editor integration and user experience.
- Community Engagement: Documenting the updates, offering support, and collaborating with maintainers and users.


### Compare your own project with existing or historical efforts (4000c)

Over the years there have been multiple tries to improve nix language tooling:

- Nixd: Originated as a Rust-based extension to the rust parser crate. It provides autocompletion for Nix option fields, has lazy package evaluation and advanced caching mechanims. However, it does not have a type inference algorithm.
- Nil: Already provides several functionalities, including option renaming, dead code analysis, and basic refactoring as well as code navigation tools. However, the current Hindely-Milner style type inferce can be improved by the SimpleSub algorithm.
- Nickel: A standalone language that aims to replace Nix entirely. It offers a more modern syntax and additional features but Nickel’s adoption would require significant migration efforts for existing Nix projects.

This project aims to improve the developer experience by enhancing the existing Nix language with advanced type inference and expanded LSP features, focusing on incremental improvements rather than replacing the language or requiring a full overhaul of existing workflows.


### What are significant technical challenges you expect to solve during the project, if any? (5000c)

This project can be broken down into three major phases with their respective challenges:

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

The Nix ecosystem is made up of a range of loosely connected groups contributing to the project from different angles. At its foundation are the Nixpkgs maintainers—volunteers and contributors who keep the package set up to date and ensure its reliability. Core development is driven by key contributors (such as [insert names]) who actively shape the Nix language, package manager, and NixOS itself. Additionally, commercial efforts like Determinate Systems —backed by Nix's original creator, Eelco Dolstra contribute to the ecosystem. The nixos foundation is a new initiative to support the Nix ecosystem and its development.

During this project, I plan to collaborate with key stakeholders, starting with Nil’s maintainer, @oxalica, whom I’ve already contacted (though I haven’t yet received a response). I’m also looking forward to connecting with the NixOS team and becoming more engaged with the wider development community.

To share the progress and outcomes of the project, I intend to publish a blog post on the NixOS wiki, both to document the work and to invite feedback from the community.
