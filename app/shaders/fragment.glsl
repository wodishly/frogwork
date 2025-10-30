#version 410

precision highp sampler2D;

in vec2 v_uv;
out vec4 pc_fragColor;

uniform sampler2D u_frog;

void main() {
    pc_fragColor = texture(u_frog, v_uv);
}
