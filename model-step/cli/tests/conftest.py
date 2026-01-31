"""Pytest configuration and shared fixtures."""

import pytest
import sys
import os
from pathlib import Path

# Add CLI directory to path for imports
cli_dir = Path(__file__).parent.parent
sys.path.insert(0, str(cli_dir))


def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line(
        "markers", "slow: marks tests as slow (deselect with '-m \"not slow\"')"
    )
    config.addinivalue_line(
        "markers", "integration: marks tests as integration tests"
    )
    config.addinivalue_line(
        "markers", "e2e: marks tests as end-to-end tests"
    )


@pytest.fixture(scope="session")
def model_step_dir():
    """Return the model-step directory path (session-scoped)."""
    return Path(__file__).parent.parent.parent


@pytest.fixture(scope="session")
def cli_dir():
    """Return the CLI directory path."""
    return Path(__file__).parent.parent


@pytest.fixture(scope="session")
def fixtures_dir():
    """Return the fixtures directory path."""
    return Path(__file__).parent / "fixtures"


@pytest.fixture
def temp_output_dir(tmp_path):
    """Create a temporary output directory."""
    output_dir = tmp_path / "output"
    output_dir.mkdir()
    return output_dir


@pytest.fixture
def env_with_api_key(monkeypatch):
    """Set up environment with a test API key."""
    monkeypatch.setenv("ANTHROPIC_API_KEY", "sk-ant-test-key-for-testing")
    monkeypatch.setenv("ANTHROPIC_MODEL", "claude-sonnet-4-20250514")
