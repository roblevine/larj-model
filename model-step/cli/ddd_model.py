#!/usr/bin/env python3
"""CLI tool for generating DDD domain models from markdown descriptions."""

import os
import sys
import tempfile
from pathlib import Path

import click
from dotenv import load_dotenv
import anthropic

from prompts import SYSTEM_PROMPT, USER_PROMPT_TEMPLATE
from prolog_runner import PrologRunner


def get_model_step_dir() -> Path:
    """Get the model-step directory (parent of cli/)."""
    return Path(__file__).parent.parent.resolve()


def load_api_key() -> str:
    """Load Anthropic API key from environment."""
    # Load from .env file in model-step directory
    env_file = get_model_step_dir() / ".env"
    load_dotenv(env_file)

    api_key = os.getenv("ANTHROPIC_API_KEY")
    if not api_key:
        raise click.ClickException(
            "ANTHROPIC_API_KEY not found. Create a .env file in model-step/ "
            "with your API key. See .env.example for format."
        )
    return api_key


def get_model_name() -> str:
    """Get the Claude model to use."""
    return os.getenv("ANTHROPIC_MODEL", "claude-sonnet-4-20250514")


def generate_prolog_model(markdown_content: str, verbose: bool = False) -> str:
    """Use Claude to generate Prolog model from markdown."""
    api_key = load_api_key()
    model = get_model_name()

    if verbose:
        click.echo(f"Using model: {model}")

    client = anthropic.Anthropic(api_key=api_key)

    user_prompt = USER_PROMPT_TEMPLATE.format(domain_content=markdown_content)

    if verbose:
        click.echo("Sending request to Claude...")

    message = client.messages.create(
        model=model,
        max_tokens=8192,
        messages=[
            {"role": "user", "content": user_prompt}
        ],
        system=SYSTEM_PROMPT
    )

    # Extract the text content
    response_text = message.content[0].text

    # Clean up - remove markdown code fences if present
    if response_text.startswith("```prolog"):
        response_text = response_text[9:]
    elif response_text.startswith("```"):
        response_text = response_text[3:]
    if response_text.endswith("```"):
        response_text = response_text[:-3]

    return response_text.strip()


@click.command()
@click.argument("markdown_file", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--output", "-o",
    type=click.Path(path_type=Path),
    help="Output Prolog file (default: output/<name>_model.pl)"
)
@click.option(
    "--name", "-n",
    default=None,
    help="Model name (default: derived from input filename)"
)
@click.option(
    "--visualize/--no-visualize", "-v/-V",
    default=True,
    help="Generate Graphviz visualizations (default: yes)"
)
@click.option(
    "--validate/--no-validate",
    default=True,
    help="Run model validation (default: yes)"
)
@click.option(
    "--verbose", is_flag=True,
    help="Show verbose output"
)
@click.option(
    "--prolog-only", is_flag=True,
    help="Only output the generated Prolog code (no file operations)"
)
def main(
    markdown_file: Path,
    output: Path | None,
    name: str | None,
    visualize: bool,
    validate: bool,
    verbose: bool,
    prolog_only: bool
):
    """Generate a DDD domain model from a markdown file.

    Takes a markdown file describing a domain and uses Claude to generate
    a Prolog-based domain model. Optionally generates Graphviz visualizations.

    Example:
        python ddd_model.py domain_description.md -v

    """
    model_step_dir = get_model_step_dir()

    # Derive model name from filename if not provided
    if name is None:
        name = markdown_file.stem.replace("-", "_").replace(" ", "_")

    # Set default output path
    if output is None:
        output = model_step_dir / "output" / f"{name}_model.pl"

    # Read markdown content
    if verbose:
        click.echo(f"Reading: {markdown_file}")

    markdown_content = markdown_file.read_text()

    # Generate Prolog model using Claude
    click.echo("Generating domain model with Claude...")
    try:
        prolog_code = generate_prolog_model(markdown_content, verbose)
    except anthropic.APIError as e:
        raise click.ClickException(f"API error: {e}")

    if prolog_only:
        click.echo(prolog_code)
        return

    # Ensure output directory exists
    output.parent.mkdir(parents=True, exist_ok=True)

    # Write Prolog file
    output.write_text(prolog_code)
    click.echo(f"Generated: {output}")

    # Run Prolog pipeline
    if visualize or validate:
        click.echo("\nProcessing with Prolog...")
        try:
            runner = PrologRunner(model_step_dir)
        except RuntimeError as e:
            raise click.ClickException(str(e))

        if visualize:
            results = runner.full_pipeline(output, name)

            if results["errors"]:
                click.echo(click.style("\nErrors:", fg="red"))
                for err in results["errors"]:
                    click.echo(f"  {err}")

            if results["dot_file"]:
                click.echo(f"\nDOT file: {results['dot_file']}")
            if results["png_file"]:
                click.echo(f"PNG file: {results['png_file']}")
            if results["svg_file"]:
                click.echo(f"SVG file: {results['svg_file']}")
            if results["context_map"]:
                click.echo(f"Context map: {results['context_map']}")

            if results["validate"] and verbose:
                click.echo("\nValidation results:")
                click.echo(results["validate"]["output"])

        elif validate:
            success, output_text = runner.validate_model(output)
            click.echo("\nValidation results:")
            click.echo(output_text)

    click.echo(click.style("\nDone!", fg="green"))


if __name__ == "__main__":
    main()
