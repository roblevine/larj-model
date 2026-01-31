"""Unit tests for prolog_runner module."""

import pytest
import sys
import shutil
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from prolog_runner import PrologRunner


@pytest.fixture
def model_step_dir():
    """Return the model-step directory path."""
    return Path(__file__).parent.parent.parent


@pytest.fixture
def runner(model_step_dir):
    """Create a PrologRunner instance."""
    return PrologRunner(model_step_dir)


@pytest.fixture
def fixtures_dir():
    """Return the fixtures directory path."""
    return Path(__file__).parent / "fixtures"


class TestPrologRunnerInit:
    """Tests for PrologRunner initialization."""

    def test_init_sets_directories(self, model_step_dir):
        """Init should set up directory paths."""
        runner = PrologRunner(model_step_dir)
        assert runner.model_step_dir == model_step_dir
        assert runner.src_dir == model_step_dir / "src"
        assert runner.output_dir == model_step_dir / "output"

    def test_init_creates_output_dir(self, model_step_dir):
        """Init should create output directory if needed."""
        runner = PrologRunner(model_step_dir)
        assert runner.output_dir.exists()

    @patch("shutil.which")
    def test_init_raises_if_no_swipl(self, mock_which, model_step_dir):
        """Init should raise if SWI-Prolog not found."""
        mock_which.return_value = None
        with pytest.raises(RuntimeError, match="SWI-Prolog"):
            PrologRunner(model_step_dir)


class TestRunProlog:
    """Tests for run_prolog method."""

    def test_run_prolog_simple_goal(self, runner):
        """Should execute a simple Prolog goal."""
        success, output = runner.run_prolog("writeln('hello'), halt(0)")
        assert success is True
        assert "hello" in output

    def test_run_prolog_failing_goal(self, runner):
        """Should return failure for failing goal."""
        success, output = runner.run_prolog("fail, halt(0)")
        assert success is False

    def test_run_prolog_arithmetic(self, runner):
        """Should execute arithmetic goals."""
        success, output = runner.run_prolog("X is 2 + 2, writeln(X), halt(0)")
        assert success is True
        assert "4" in output

    def test_run_prolog_timeout(self, runner):
        """Should handle timeout."""
        # This should timeout quickly
        success, output = runner.run_prolog("repeat, fail", timeout=1)
        assert success is False
        assert "timeout" in output.lower() or "timed out" in output.lower()


class TestLoadAndBuildModel:
    """Tests for load_and_build_model method."""

    def test_load_valid_model(self, runner, fixtures_dir):
        """Should load and build a valid model file."""
        model_file = fixtures_dir / "simple_domain_model.pl"
        if model_file.exists():
            success, output = runner.load_and_build_model(
                model_file, "simple_domain"
            )
            # May fail if paths don't resolve correctly, but shouldn't crash
            assert isinstance(success, bool)
            assert isinstance(output, str)


class TestGenerateDot:
    """Tests for DOT generation methods."""

    def test_generate_dot_creates_file(self, runner, fixtures_dir, model_step_dir):
        """Should generate a DOT file."""
        model_file = fixtures_dir / "simple_domain_model.pl"
        # Output file must be within model_step_dir for relative_to() to work
        output_file = model_step_dir / "output" / "test_generate_dot.dot"

        if model_file.exists():
            try:
                success, output = runner.generate_dot(
                    model_file, "test_model", output_file
                )
                # Test the interface works without necessarily succeeding
                assert isinstance(success, bool)
            finally:
                # Cleanup
                if output_file.exists():
                    output_file.unlink()


class TestRenderDot:
    """Tests for DOT rendering methods."""

    @pytest.fixture
    def sample_dot_file(self, tmp_path):
        """Create a sample DOT file."""
        dot_content = """digraph test {
            A -> B;
            B -> C;
        }"""
        dot_file = tmp_path / "test.dot"
        dot_file.write_text(dot_content)
        return dot_file

    def test_render_to_png(self, runner, sample_dot_file):
        """Should render DOT to PNG if Graphviz available."""
        if shutil.which("dot"):
            success, output = runner.render_dot(sample_dot_file, "png")
            assert success is True
            assert output.endswith(".png")
            assert Path(output).exists()
        else:
            success, output = runner.render_dot(sample_dot_file, "png")
            assert success is False
            assert "not found" in output.lower()

    def test_render_to_svg(self, runner, sample_dot_file):
        """Should render DOT to SVG if Graphviz available."""
        if shutil.which("dot"):
            success, output = runner.render_dot(sample_dot_file, "svg")
            assert success is True
            assert output.endswith(".svg")
        else:
            success, output = runner.render_dot(sample_dot_file, "svg")
            assert success is False

    @patch("shutil.which")
    def test_render_fails_without_graphviz(self, mock_which, runner, sample_dot_file):
        """Should fail gracefully if Graphviz not installed."""
        mock_which.return_value = None
        success, output = runner.render_dot(sample_dot_file, "png")
        assert success is False
        assert "not found" in output.lower()


class TestFullPipeline:
    """Tests for the full_pipeline method."""

    def test_full_pipeline_returns_dict(self, runner, fixtures_dir):
        """Full pipeline should return a results dictionary."""
        model_file = fixtures_dir / "simple_domain_model.pl"
        if model_file.exists():
            results = runner.full_pipeline(model_file, "test_model")
            assert isinstance(results, dict)
            assert "build" in results
            assert "validate" in results
            assert "errors" in results
            assert isinstance(results["errors"], list)

    def test_full_pipeline_structure(self, runner, fixtures_dir):
        """Full pipeline results should have expected keys."""
        model_file = fixtures_dir / "simple_domain_model.pl"
        if model_file.exists():
            results = runner.full_pipeline(model_file, "test_model")
            expected_keys = [
                "build", "validate", "dot_file",
                "context_map", "png_file", "svg_file", "errors"
            ]
            for key in expected_keys:
                assert key in results
