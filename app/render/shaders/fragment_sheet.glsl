#version 410

in vec2 v_uv;
out vec4 fColor;

void main() {
    fColor = vec4(vec2(0.48, 0.27) * v_uv, 0.48, 1.0);
}
