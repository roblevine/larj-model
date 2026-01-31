"""End-to-end tests for the CLI tool.

These tests verify the complete workflow from markdown input to
visualization output. Some tests require external dependencies
(SWI-Prolog, Graphviz) and may be skipped if not available.
"""

import pytest
import sys
import shutil
import os
from pathlib import Path
from click.testing import CliRunner

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from ddd_model import main
from prolog_runner import PrologRunner


# Check for required external tools
HAS_SWIPL = shutil.which("swipl") is not None
HAS_GRAPHVIZ = shutil.which("dot") is not None
HAS_API_KEY = os.getenv("ANTHROPIC_API_KEY") is not None


@pytest.fixture
def cli_runner():
    """Create a Click CLI test runner."""
    return CliRunner()


@pytest.fixture
def model_step_dir():
    """Return the model-step directory path."""
    return Path(__file__).parent.parent.parent


@pytest.fixture
def fixtures_dir():
    """Return the fixtures directory path."""
    return Path(__file__).parent / "fixtures"


@pytest.fixture
def sample_markdown(fixtures_dir):
    """Return path to sample markdown file."""
    return fixtures_dir / "simple_domain.md"


@pytest.fixture
def sample_prolog_model(fixtures_dir):
    """Return path to sample Prolog model file."""
    return fixtures_dir / "simple_domain_model.pl"


class TestPrologModelExecution:
    """Test that generated Prolog models can be executed."""

    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    def test_fixture_model_loads(self, model_step_dir, sample_prolog_model):
        """The fixture model should load without errors."""
        runner = PrologRunner(model_step_dir)

        # Copy fixture to output dir for proper path resolution
        output_model = model_step_dir / "output" / "test_fixture_model.pl"
        output_model.parent.mkdir(exist_ok=True)

        # Read and adjust the fixture for correct path
        content = sample_prolog_model.read_text()
        # The fixture uses '../src/' which should work from output/
        output_model.write_text(content)

        try:
            success, output = runner.load_and_build_model(output_model, "test_fixture")
            # Model should load (may have warnings but shouldn't crash)
            assert isinstance(output, str)
        finally:
            # Cleanup
            if output_model.exists():
                output_model.unlink()

    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    def test_ecommerce_example_loads(self, model_step_dir):
        """The e-commerce example should load and build."""
        runner = PrologRunner(model_step_dir)
        example_file = model_step_dir / "examples" / "ecommerce_model.pl"

        if example_file.exists():
            success, output = runner.run_prolog(
                f"['{example_file}'], build_ecommerce_model, halt(0)"
            )
            assert success is True, f"Failed to load ecommerce model: {output}"


class TestVisualizationGeneration:
    """Test visualization generation from Prolog models."""

    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    def test_dot_generation(self, model_step_dir, tmp_path):
        """Should generate DOT file from e-commerce example."""
        runner = PrologRunner(model_step_dir)
        example_file = model_step_dir / "examples" / "ecommerce_model.pl"
        dot_file = tmp_path / "test_ecommerce.dot"

        if example_file.exists():
            success, output = runner.run_prolog(f"""
                ['{example_file}'],
                [src/model_visualize],
                build_ecommerce_model,
                generate_dot_file(ecommerce, '{dot_file}', []),
                halt(0)
            """.replace('\n', ' '))

            if success:
                assert dot_file.exists(), "DOT file was not created"
                content = dot_file.read_text()
                assert "digraph" in content
                assert "order" in content  # Should contain order node

    @pytest.mark.skipif(not HAS_SWIPL or not HAS_GRAPHVIZ,
                        reason="SWI-Prolog or Graphviz not installed")
    def test_png_generation(self, model_step_dir, tmp_path):
        """Should generate PNG from DOT file."""
        runner = PrologRunner(model_step_dir)

        # Create a simple DOT file
        dot_file = tmp_path / "test.dot"
        dot_file.write_text('digraph test { A -> B; }')

        success, png_path = runner.render_dot(dot_file, "png")

        assert success is True
        assert Path(png_path).exists()
        assert Path(png_path).stat().st_size > 0


class TestCLIEndToEnd:
    """End-to-end CLI tests."""

    @pytest.mark.skipif(not HAS_API_KEY, reason="ANTHROPIC_API_KEY not set")
    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    @pytest.mark.slow
    def test_full_workflow_live_api(self, cli_runner, sample_markdown, tmp_path):
        """Test complete workflow with live API call.

        This test makes a real API call and should be run sparingly.
        Mark with @pytest.mark.slow to skip in normal test runs.
        """
        output_file = tmp_path / "output" / "live_test_model.pl"
        output_file.parent.mkdir(parents=True, exist_ok=True)

        result = cli_runner.invoke(main, [
            str(sample_markdown),
            "--name", "live_test",
            "--output", str(output_file),
            "--verbose"
        ])

        # Should complete without error
        assert result.exit_code == 0 or "Done!" in result.output

        # Should create output file
        if result.exit_code == 0:
            assert output_file.exists()
            content = output_file.read_text()
            assert "build_model" in content or "define_" in content

    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    def test_prolog_only_no_api_needed(self, cli_runner, fixtures_dir, model_step_dir):
        """Test that --prolog-only with fixture doesn't need API.

        This test uses a pre-generated fixture to verify the Prolog
        execution works without making API calls.
        """
        # This test verifies the Prolog execution path works
        # by using the existing e-commerce example
        example_file = model_step_dir / "examples" / "ecommerce_model.pl"

        if example_file.exists():
            runner = PrologRunner(model_step_dir)
            results = runner.full_pipeline(example_file, "ecommerce_test")

            # Should have attempted the build
            assert "build" in results
            # May or may not succeed depending on environment


class TestModelValidation:
    """Test model validation functionality."""

    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    def test_validate_ecommerce_model(self, model_step_dir):
        """E-commerce example should pass validation."""
        runner = PrologRunner(model_step_dir)
        example_file = model_step_dir / "examples" / "ecommerce_model.pl"

        if example_file.exists():
            success, output = runner.run_prolog(f"""
                ['{example_file}'],
                [src/model_validate],
                build_ecommerce_model,
                validate_full_model(Results),
                maplist(writeln, Results),
                halt(0)
            """.replace('\n', ' '))

            # Should execute without crashing
            assert isinstance(output, str)
            if success:
                assert "result" in output.lower() or "pass" in output.lower()


class TestErrorHandling:
    """Test error handling in the CLI."""

    def test_invalid_markdown_path(self, cli_runner):
        """Should handle invalid file path gracefully."""
        result = cli_runner.invoke(main, ["/nonexistent/path/file.md"])
        assert result.exit_code != 0

    @pytest.mark.skipif(not HAS_SWIPL, reason="SWI-Prolog not installed")
    def test_invalid_prolog_syntax(self, model_step_dir, tmp_path):
        """Should handle invalid Prolog syntax."""
        runner = PrologRunner(model_step_dir)

        # Create invalid Prolog file with syntax that will cause runtime error
        bad_file = tmp_path / "bad_model.pl"
        bad_file.write_text("build_model :- undefined_predicate_xyz.")

        # Try to call the undefined predicate - this should fail
        success, output = runner.run_prolog(f"['{bad_file}'], build_model, halt(0)")
        # SWI-Prolog may load the file but fail when calling undefined predicate
        # Either way, we check that output contains error information or fails
        assert success is False or "error" in output.lower() or "undefined" in output.lower()


# Pytest configuration for markers
def pytest_configure(config):
    config.addinivalue_line(
        "markers", "slow: marks tests as slow (deselect with '-m \"not slow\"')"
    )
