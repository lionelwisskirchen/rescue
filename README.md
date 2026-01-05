
# Table of Contents

1.  [ReScUE - REproducible Scientific User Environment](#org7cbfc48)
    1.  [Components](#orga31a97b)
        1.  [Nix](#org13f7b46)
        2.  [Emacs](#org1db9419)
    2.  [Installation](#orgc1c69a1)
        1.  [System-wide Installation](#org3d14086)
        2.  [Docker-based Installation](#org90057f2)
    3.  [Usage](#org30120f2)
-   [References ](#orgaecf991)


<a id="org7cbfc48"></a>

# ReScUE - REproducible Scientific User Environment

This project provides a fully reproducible scientific writing environment built on ****Emacs**** and ****Nix****, combining structured text authoring, integrated reference management, and a networked knowledge base. By leveraging Nix, Flakes, and Home Manager, the entire environment, including all dependencies, configurations, and Emacs packages, is version-controlled and can be reproduced identically on any machine. The key benefits of the environment include:

-   ****Reproducibility:**** All software and user configurations are declarative and isolated, ensuring the same environment can be rebuilt in the future or shared with collaborators.
-   ****Integrated workflow:**** Org-mode, Org-Citar, and Org-Roam provide seamless support for structured writing, citations, and knowledge management.
-   ****Flexibility:**** The environment supports exporting to PDF via LaTeX, embedding images, tables, and charts, and adding footnotes or other scholarly annotations easily.
-   ****Extensibility:**** Users can extend the workflow with custom Emacs packages or additional Nix-managed tools, without breaking reproducibility.


<a id="orga31a97b"></a>

## Components

This environment is built from several layers, each ensuring reproducibility and collaboration. 
The following sections describe each component and its role in the reproducible scientific writing workflow.


<a id="org13f7b46"></a>

### Nix

Nix is a declarative package and system manager that enables the complete and reproducible specification of software environments (<a href="#citeproc_bib_item_6"><i>Nix &#38; NixOS | Declarative Builds and Deployments</i>, n.d.</a>). All dependencies including the LaTeX toolchain, Emacs packages and auxiliary tools are defined in a single configuration and are isolated from the host system. This guarantees that the same writing environment can be rebuilt identically on any machine, independent of system libraries, operating system state, or user configuration. This strict separation of configuration and execution forms the foundation of a stable and reproducible scientific writing workflow.

1.  Flakes

    Nix Flakes provide a modern, structured approach to managing Nix configurations. They introduce explicit dependency pinning, clear input/output interfaces, and reproducible builds based on version-controlled revisions. In this project, Flakes are used to lock the writing environment, including Emacs, its extensions, and the LaTeX stack, to specific software versions. This ensures that a document can be reopened, compiled, and edited years later in exactly the same environment in which it was originally created. (<a href="#citeproc_bib_item_2"><i>Flakes - NixOS Wiki</i>, n.d.</a>)

2.  Home Manager

    Home Manager provides declarative, user-level configuration within the Nix ecosystem. It manages dotfiles, application settings, and development tools as part of the same reproducible specification as the software environment. In this project, it ensures that not only the installed programs, but also their configurations, including Emacs and supporting utilities, can be restored consistently across different machines. (<a href="#citeproc_bib_item_5"><i>Nix-Community/Home-Manager:</i>, 2026</a>)


<a id="org1db9419"></a>

### Emacs

Emacs serves as the central workspace, integrating text authoring, knowledge organization, and reference management within a single extensible tool. Thanks to its modular architecture, Emacs can function as a “writing operating system” that supports complex scientific workflows. Combined with Nix, the Emacs configuration and installed packages are also version-stable, enabling transparent, reproducible editing environments across systems and collaborators. Since some Emacs packages rely on system-level dependencies (for example LaTeX packages), these dependencies can affect or break the environment. Nix provides a declarative way to manage these dependencies and ensure a stable, reproducible setup. (<a href="#citeproc_bib_item_4"><i>GNU Emacs - GNU Project</i>, n.d.</a>)

1.  Org-Mode

    Org-mode is the core of the writing process. Texts are written in a lightweight, structured markup format that serves both as an authoring medium and as a source for export pipelines. Org-mode supports figures, tables, source code blocks and it can export documents to LaTeX, PDF, HTML and several more. This approach decouples the writing process from the final publication format while preserving structure and traceability. (<a href="#citeproc_bib_item_7"><i>Org Mode</i>, n.d.</a>)

2.  Org-Citar

    Org-Citar provides integrated reference management within the Org-mode workflow. It works with external BibTeX or CSL-JSON libraries and offers citation insertion, bibliography management, and contextual lookup directly inside Emacs. References remain machine-readable, version-controlled, and project-local, avoiding dependence on proprietary database systems while supporting collaboration and archival stability. (<a href="#citeproc_bib_item_3"><i>GitHub - Emacs-Citar/Citar</i>, n.d.</a>)

3.  Org-Roam

    Org-Roam extends the environment with a networked knowledge base inspired by the *Zettelkasten* method. Research notes, conceptual fragments, and literature annotations can be interlinked, forming an evolving graph of ideas that supports long-term scholarly thinking and writing. Within the setup, Org-Roam contributes not only to document production but also to the structured development of scientific arguments over time.
    (<a href="#citeproc_bib_item_8"><i>Org-Roam</i>, n.d.</a>)


<a id="orgc1c69a1"></a>

## Installation

To set up the reproducible scientific writing environment, you first need to install the Nix package manager. The following sections describe the installation process.


<a id="org3d14086"></a>

### System-wide Installation

It can be installed as either a multi-user or single-user installation (<a href="#citeproc_bib_item_1"><i>Download | Nix &#38; NixOS</i>, n.d.</a>).
After installation, activate the environment variables:

    . ~/.nix-profile/etc/profile.d/nix.sh

Since the environment is based on Nix flakes, you must enable them:

    mkdir -p ~/.config/nix/ && echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf

This creates the necessary Nix configuration file and enables flake support.
Next, build and activate the rescue Home Manager configuration:

    nix run github:nix-community/home-manager -- switch --flake .#rescue

This command installs and activates the reproducible writing environment. Once done, you can start Emacs and the environment should work.


<a id="org90057f2"></a>

### Docker-based Installation

This repository also provides a Dockerfile for testing, but GUI support is not included and some functionalities may not work. To use it install docker and run:

    docker build --network=host -t rescue .
    docker run -it --rm rescue
    . ~/.nix-profile/etc/profile.d/nix.sh


<a id="org30120f2"></a>

## Usage

To test the environment, open the provided [template](template/Example.org) in Emacs:

    C-x f template/Example.org


<a id="orgaecf991"></a>

# References 

<div class="csl-bib-body">
  <div class="csl-entry"><a id="citeproc_bib_item_1"></a><i>Download | Nix &#38; NixOS</i>. (n.d.). Retrieved January 5, 2026 from <a href="https://nixos.org/download/">https://nixos.org/download/</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_2"></a><i>Flakes - NixOS Wiki</i>. (n.d.). Retrieved January 5, 2026 from <a href="https://nixos.wiki/wiki/Flakes">https://nixos.wiki/wiki/Flakes</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_3"></a><i>GitHub - emacs-citar/citar</i>. (n.d.). Retrieved October 16, 2025 from <a href="https://github.com/emacs-citar/citar">https://github.com/emacs-citar/citar</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_4"></a><i>GNU Emacs - GNU Project</i>. (n.d.). Retrieved October 16, 2025 from <a href="https://www.gnu.org/software/emacs/emacs.html">https://www.gnu.org/software/emacs/emacs.html</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_5"></a><i>Nix-community/home-manager:</i> (2026). Retrieved January 5, 2026 from <a href="https://github.com/nix-community/home-manager">https://github.com/nix-community/home-manager</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_6"></a><i>Nix &#38; NixOS | Declarative builds and deployments</i>. (n.d.). Retrieved October 16, 2025 from <a href="https://nixos.org/">https://nixos.org/</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_7"></a><i>Org Mode</i>. (n.d.). Retrieved October 16, 2025 from <a href="https://orgmode.org">https://orgmode.org</a></div>
  <div class="csl-entry"><a id="citeproc_bib_item_8"></a><i>Org-roam</i>. (n.d.). Retrieved October 16, 2025 from <a href="https://www.orgroam.com/">https://www.orgroam.com/</a></div>
</div>

\newpage

