#version 410

in vec3 position;
out vec2 v_uv;

void main() {
  v_uv = vec2(0.5 + position.x / 2.0, -0.5 + position.y * -2.0);
  gl_Position = vec4(position, 1.0);
}

// (-1, -1/4) -> (0, 0)
// (1, -3/4) -> (1, 1)