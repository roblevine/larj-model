"""Interface to run Prolog commands and generate visualizations."""

import subprocess
import tempfile
import shutil
from pathlib import Path


class PrologRunner:
    """Runs Prolog commands using SWI-Prolog."""

    def __init__(self, model_step_dir: Path):
        self.model_step_dir = model_step_dir
        self.src_dir = model_step_dir / "src"
        self.output_dir = model_step_dir / "output"
        self.output_dir.mkdir(exist_ok=True)

        # Check for swipl
        if not shutil.which("swipl"):
            raise RuntimeError("SWI-Prolog (swipl) not found. Please install it.")

    def run_prolog(self, goal: str, timeout: int = 60) -> tuple[bool, str]:
        """Run a Prolog goal and return (success, output)."""
        cmd = [
            "swipl",
            "-g", goal,
            "-t", "halt(1)"
        ]
        try:
            result = subprocess.run(
                cmd,
                cwd=str(self.model_step_dir),
                capture_output=True,
                text=True,
                timeout=timeout
            )
            output = result.stdout + result.stderr
            return result.returncode == 0, output
        except subprocess.TimeoutExpired:
            return False, "Prolog execution timed out"
        except Exception as e:
            return False, f"Error running Prolog: {e}"

    def load_and_build_model(self, model_file: Path, model_name: str) -> tuple[bool, str]:
        """Load a model file and build the model."""
        # Resolve to absolute path, then convert to relative from model_step_dir
        abs_path = model_file.resolve()
        rel_path = abs_path.relative_to(self.model_step_dir)
        goal = f"['{rel_path}'], build_model, halt(0)"
        return self.run_prolog(goal)

    def validate_model(self, model_file: Path) -> tuple[bool, str]:
        """Run validation on a loaded model."""
        abs_path = model_file.resolve()
        rel_path = abs_path.relative_to(self.model_step_dir)
        goal = f"""
            ['{rel_path}'],
            [src/model_validate],
            build_model,
            validate_full_model(Results),
            maplist(writeln, Results),
            halt(0)
        """
        return self.run_prolog(goal.replace('\n', ' '))

    def generate_dot(self, model_file: Path, model_name: str, output_file: Path) -> tuple[bool, str]:
        """Generate DOT visualization file."""
        abs_model = model_file.resolve()
        abs_output = output_file.resolve()
        rel_model = abs_model.relative_to(self.model_step_dir)
        rel_output = abs_output.relative_to(self.model_step_dir)
        goal = f"""
            ['{rel_model}'],
            [src/model_visualize],
            build_model,
            generate_dot_file({model_name}, '{rel_output}', [show_attributes(true), show_behaviours(true)]),
            halt(0)
        """
        return self.run_prolog(goal.replace('\n', ' '))

    def generate_context_map(self, model_file: Path, model_name: str, output_file: Path) -> tuple[bool, str]:
        """Generate context map DOT file."""
        abs_model = model_file.resolve()
        abs_output = output_file.resolve()
        rel_model = abs_model.relative_to(self.model_step_dir)
        rel_output = abs_output.relative_to(self.model_step_dir)
        goal = f"""
            ['{rel_model}'],
            [src/model_visualize],
            build_model,
            generate_context_map_dot({model_name}, DotCode),
            open('{rel_output}', write, S),
            write(S, DotCode),
            close(S),
            halt(0)
        """
        return self.run_prolog(goal.replace('\n', ' '))

    def render_dot(self, dot_file: Path, output_format: str = "png") -> tuple[bool, str]:
        """Render DOT file to image using Graphviz."""
        if not shutil.which("dot"):
            return False, "Graphviz (dot) not found. Install with: brew install graphviz"

        output_file = dot_file.with_suffix(f".{output_format}")
        cmd = ["dot", f"-T{output_format}", str(dot_file), "-o", str(output_file)]

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
            if result.returncode == 0:
                return True, str(output_file)
            return False, result.stderr
        except Exception as e:
            return False, f"Error rendering: {e}"

    def full_pipeline(self, model_file: Path, model_name: str) -> dict:
        """Run the full pipeline: build, validate, visualize."""
        results = {
            "build": None,
            "validate": None,
            "dot_file": None,
            "context_map": None,
            "png_file": None,
            "svg_file": None,
            "errors": []
        }

        # Build model
        success, output = self.load_and_build_model(model_file, model_name)
        results["build"] = {"success": success, "output": output}
        if not success:
            results["errors"].append(f"Build failed: {output}")
            return results

        # Validate
        success, output = self.validate_model(model_file)
        results["validate"] = {"success": success, "output": output}

        # Generate DOT
        dot_file = self.output_dir / f"{model_name}.dot"
        success, output = self.generate_dot(model_file, model_name, dot_file)
        if success:
            results["dot_file"] = str(dot_file)

            # Render to PNG
            success, png_path = self.render_dot(dot_file, "png")
            if success:
                results["png_file"] = png_path

            # Render to SVG
            success, svg_path = self.render_dot(dot_file, "svg")
            if success:
                results["svg_file"] = svg_path
        else:
            results["errors"].append(f"DOT generation failed: {output}")

        # Generate context map
        ctx_map_file = self.output_dir / f"{model_name}_context_map.dot"
        success, output = self.generate_context_map(model_file, model_name, ctx_map_file)
        if success:
            results["context_map"] = str(ctx_map_file)
            # Render context map
            self.render_dot(ctx_map_file, "png")

        return results
