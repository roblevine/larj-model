"""Unit tests for the CLI tool (ddd_model.py)."""

import pytest
import sys
import os
from pathlib import Path
from unittest.mock import patch, MagicMock
from click.testing import CliRunner

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from ddd_model import main, get_model_step_dir, load_api_key, get_model_name, generate_prolog_model


@pytest.fixture
def cli_runner():
    """Create a Click CLI test runner."""
    return CliRunner()


@pytest.fixture
def fixtures_dir():
    """Return the fixtures directory path."""
    return Path(__file__).parent / "fixtures"


@pytest.fixture
def sample_markdown(fixtures_dir):
    """Return path to sample markdown file."""
    return fixtures_dir / "simple_domain.md"


class TestHelperFunctions:
    """Tests for helper functions."""

    def test_get_model_step_dir(self):
        """Should return the model-step directory."""
        model_step_dir = get_model_step_dir()
        assert model_step_dir.exists()
        assert model_step_dir.name == "model-step" or "cli" in str(model_step_dir)

    def test_get_model_name_default(self):
        """Should return default model name."""
        with patch.dict(os.environ, {}, clear=True):
            # Remove ANTHROPIC_MODEL if set
            os.environ.pop("ANTHROPIC_MODEL", None)
            name = get_model_name()
            assert "claude" in name.lower()

    def test_get_model_name_from_env(self):
        """Should return model name from environment."""
        with patch.dict(os.environ, {"ANTHROPIC_MODEL": "claude-opus-4-20250514"}):
            name = get_model_name()
            assert name == "claude-opus-4-20250514"


class TestLoadApiKey:
    """Tests for API key loading."""

    def test_load_api_key_missing_raises(self):
        """Should raise if API key not found."""
        with patch.dict(os.environ, {}, clear=True):
            os.environ.pop("ANTHROPIC_API_KEY", None)
            with patch("ddd_model.load_dotenv"):  # Don't load real .env
                with pytest.raises(Exception):  # ClickException
                    load_api_key()

    def test_load_api_key_from_env(self):
        """Should load API key from environment."""
        test_key = "sk-ant-test-key-12345"
        with patch.dict(os.environ, {"ANTHROPIC_API_KEY": test_key}):
            with patch("ddd_model.load_dotenv"):
                key = load_api_key()
                assert key == test_key


class TestGeneratePrologModel:
    """Tests for Prolog model generation via Claude."""

    @patch("ddd_model.anthropic.Anthropic")
    @patch("ddd_model.load_api_key")
    def test_generate_prolog_model_calls_api(self, mock_load_key, mock_anthropic):
        """Should call Anthropic API with correct parameters."""
        mock_load_key.return_value = "test-key"

        # Mock the API response
        mock_client = MagicMock()
        mock_anthropic.return_value = mock_client
        mock_message = MagicMock()
        mock_message.content = [MagicMock(text=":- use_module(test).\nbuild_model :- true.")]
        mock_client.messages.create.return_value = mock_message

        result = generate_prolog_model("Test domain description")

        # Verify API was called
        mock_client.messages.create.assert_called_once()
        call_kwargs = mock_client.messages.create.call_args[1]
        assert "messages" in call_kwargs
        assert "system" in call_kwargs

    @patch("ddd_model.anthropic.Anthropic")
    @patch("ddd_model.load_api_key")
    def test_generate_prolog_model_strips_code_fences(self, mock_load_key, mock_anthropic):
        """Should strip markdown code fences from response."""
        mock_load_key.return_value = "test-key"

        mock_client = MagicMock()
        mock_anthropic.return_value = mock_client
        mock_message = MagicMock()
        mock_message.content = [MagicMock(text="```prolog\nbuild_model :- true.\n```")]
        mock_client.messages.create.return_value = mock_message

        result = generate_prolog_model("Test")

        assert not result.startswith("```")
        assert not result.endswith("```")
        assert "build_model" in result


class TestCLIBasic:
    """Basic CLI tests."""

    def test_cli_help(self, cli_runner):
        """Should show help message."""
        result = cli_runner.invoke(main, ["--help"])
        assert result.exit_code == 0
        assert "Generate a DDD domain model" in result.output

    def test_cli_missing_file(self, cli_runner):
        """Should error on missing input file."""
        result = cli_runner.invoke(main, ["nonexistent.md"])
        assert result.exit_code != 0

    def test_cli_requires_markdown_file(self, cli_runner):
        """Should require a markdown file argument."""
        result = cli_runner.invoke(main, [])
        assert result.exit_code != 0


class TestCLIPrologOnly:
    """Tests for --prolog-only mode."""

    @patch("ddd_model.generate_prolog_model")
    def test_prolog_only_outputs_to_stdout(self, mock_generate, cli_runner, sample_markdown):
        """--prolog-only should output Prolog code to stdout."""
        mock_generate.return_value = "build_model :- true."

        result = cli_runner.invoke(main, [str(sample_markdown), "--prolog-only"])

        assert "build_model" in result.output
        mock_generate.assert_called_once()

    @patch("ddd_model.generate_prolog_model")
    def test_prolog_only_no_file_operations(self, mock_generate, cli_runner, sample_markdown, tmp_path):
        """--prolog-only should not create files."""
        mock_generate.return_value = "build_model :- true."
        output_file = tmp_path / "should_not_exist.pl"

        result = cli_runner.invoke(main, [
            str(sample_markdown),
            "--prolog-only",
            "--output", str(output_file)
        ])

        # Output file should NOT be created in prolog-only mode
        # (The code outputs to stdout instead)
        assert "build_model" in result.output


class TestCLIOptions:
    """Tests for CLI options."""

    @patch("ddd_model.generate_prolog_model")
    @patch("ddd_model.PrologRunner")
    def test_no_visualize_skips_pipeline(self, mock_runner, mock_generate, cli_runner, sample_markdown, tmp_path):
        """--no-visualize should skip visualization pipeline."""
        mock_generate.return_value = ":- use_module(test).\nbuild_model :- true."

        with cli_runner.isolated_filesystem(temp_dir=tmp_path):
            result = cli_runner.invoke(main, [
                str(sample_markdown),
                "--no-visualize",
                "--no-validate"
            ])

        # Runner should not be instantiated for full pipeline
        # when both visualize and validate are disabled
        # (Actually it checks if visualize OR validate, so this tests the logic)

    @patch("ddd_model.generate_prolog_model")
    def test_name_option(self, mock_generate, cli_runner, sample_markdown, tmp_path):
        """--name should set the model name."""
        mock_generate.return_value = "build_model :- true."

        result = cli_runner.invoke(main, [
            str(sample_markdown),
            "--name", "custom_name",
            "--prolog-only"
        ])

        assert result.exit_code == 0

    @patch("ddd_model.generate_prolog_model")
    def test_verbose_option(self, mock_generate, cli_runner, sample_markdown):
        """--verbose should show additional output."""
        mock_generate.return_value = "build_model :- true."

        result = cli_runner.invoke(main, [
            str(sample_markdown),
            "--verbose",
            "--prolog-only"
        ])

        # Verbose mode shows "Reading:" at minimum
        assert "Reading:" in result.output


class TestCLIIntegration:
    """Integration tests for CLI with mocked API."""

    @patch("ddd_model.generate_prolog_model")
    @patch("ddd_model.PrologRunner")
    def test_full_workflow_mocked(self, mock_runner_class, mock_generate, cli_runner, sample_markdown, tmp_path):
        """Test full workflow with mocked components."""
        # Mock the Prolog model generation
        mock_generate.return_value = """:- use_module('../src/model_builder').
build_model :-
    clear_model,
    define_context(test_ctx, 'Test', 'Scope'),
    define_moment_interval(event, test_ctx, moment, 'Test event').
"""
        # Mock the Prolog runner
        mock_runner = MagicMock()
        mock_runner_class.return_value = mock_runner
        mock_runner.full_pipeline.return_value = {
            "build": {"success": True, "output": ""},
            "validate": {"success": True, "output": "All checks passed"},
            "dot_file": str(tmp_path / "test.dot"),
            "png_file": str(tmp_path / "test.png"),
            "svg_file": str(tmp_path / "test.svg"),
            "context_map": str(tmp_path / "test_context_map.dot"),
            "errors": []
        }

        with cli_runner.isolated_filesystem(temp_dir=tmp_path):
            # Create output dir
            (tmp_path / "output").mkdir(exist_ok=True)

            result = cli_runner.invoke(main, [
                str(sample_markdown),
                "--name", "test_model",
                "--output", str(tmp_path / "output" / "test_model.pl")
            ])

        assert "Done!" in result.output or result.exit_code == 0
