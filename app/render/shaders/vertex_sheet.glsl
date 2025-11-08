#version 410

precision highp float;

layout(location = 0) in vec4 position;

uniform mat4 u_model_matrix;
uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

out vec4 v_position;

void main() {
        v_position = position;
        mat4 modelview_matrix = u_view_matrix * u_model_matrix;
        gl_Position = u_projection_matrix * modelview_matrix * position;
}
