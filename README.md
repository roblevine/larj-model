# larj-model
Larj-model is a highly experimental modelling toolset for domain modelling of organisations and enterprises.

It is intended to operate in two parts:
- elicitation and identification of core domain concepts via an LLM interface
- mapping of these concepts to a formal domain model representation using Prolog.


# Getting started
This project is in its very early stages. To get started, clone the repository.

## Prerequisites
- Claude Code
- SWI Prolog

## DevContainer setup
For convenience, a DevContainer configuration is provided, which will have all tools preinstalled. This DevContainer setup requires Docker or Podman and should work across Linux, MacOS, and Windows with WSL2 hosts. It may not fully work on native Windows environments due to the dependency on bash scripting for some DevContainer setup tasks.

To use the DevContainer setup:
1. Ensure you have Docker or Podman installed on your machine.
2. Ensure you have the DevContainer extension installed in your VSCode.
3. Open the cloned repository in VSCode.
4. When prompted by VSCode, reopen the project in the DevContainer.
5. Once the DevContainer is built and running, you can start using the tools provided in the repository, via the vs-code terminal into the DevContainer

### SSH Access
Alternatively, you can access the DevContainer via SSH.

This DevContainer uses a custom SSH setup instead of the standard DevContainer SSH feature. This is to allow for better control over the SSH configuration and to ensure compatibility across different host systems and Podman/Docker.

To enable SSH access, create a .env_devcontainer file in the .devcontainer directory, based on the provided .env_devcontainer.example file, and uncomment `ENABLE_SSH_SERVER_IN_DEVCONTAINER=true` before building the DevContainer.

Two additional quality-of-life environment variables are available for configuring the SSH environment.

`SSH_AUTHORIZED_KEY`: If you prefer key exchange to password auth (you probably should!), paste your public SSH key here. This key will be added to the authorized_keys file in the DevContainer for SSH access.

`SSH_HOST_ED25519_KEY`: After your first container rebuild, your SSH client is likely to start complaining that the remote host SSH key has changed. To avoid this, you can copy a host key into this setting and it will automatically be set on container build, so the container has a fixed deterministic key after rebuild. Leave this blank to have a new key generated on each container build - you can then copy and paste the key from the build log (or terminal) into this setting.

By default, the SSH server will listen on 2204 on the local lopoback interface of the host machine. You can change this by modifying the appPorts setting in the .devcontainer/devcontainer.json file, or directly in the "Ports" tab in the DevContainer window in VSCode.

You should be able to connect to the DevContainer via SSH using the following command:

```bash
ssh -p 2204 vscode@localhost