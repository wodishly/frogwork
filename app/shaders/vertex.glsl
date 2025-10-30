#version 410

precision highp float;

in vec2 position;

out vec2 v_uv;

uniform vec2 u_position;

void main() {
        vec2 pos = u_position / 10000.0;
        v_uv = pos / 2.0 + 1.0;

        pos.y *= -1.0;
        gl_Position = vec4(position + pos, 0.0, 1.0);
}
