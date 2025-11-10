#version 410

precision highp sampler2D;

in vec2 v_uv;
out vec4 f_color;

uniform sampler2D u_texture;

void main() {
    float color = texture(u_texture, v_uv).r;
    f_color = vec4(color);
}
